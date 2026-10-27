mock_compat <- function(fun, ...) {
  environment(fun) <- list2env(list(...), parent = environment(fun))
  fun
}

test_that("is_linux distinguishes Linux from macOS and Windows", {
  for (os in c("Linux", "Darwin", "Windows")) {
    fun <- mock_compat(Ipaper:::is_linux,
      Sys.info = function() c(sysname = os)
    )
    expect_identical(fun(), os == "Linux")
  }
})

test_that("OS_type identifies macOS and WSL", {
  fun <- mock_compat(OS_type,
    is_mac = function() TRUE,
    is_wsl = function() FALSE
  )
  expect_identical(fun(), "mac")
  fun <- mock_compat(OS_type,
    is_mac = function() FALSE,
    is_wsl = function() TRUE
  )
  expect_identical(fun(), "wsl")
})

test_that("dir.show uses the platform file manager", {
  path <- tempdir()
  for (os in c("mac", "windows", "unix", "wsl")) {
    command <- NULL
    fun <- mock_compat(dir.show,
      OS_type = function() os,
      check_path = identity,
      shell = function(cmd) command <<- cmd
    )
    fun(path)
    expected <- switch(os,
      mac = sprintf("open %s", shQuote(path)),
      windows = paste("explorer.exe /e, ", path),
      unix = sprintf("nautilus '%s'", path),
      wsl = sprintf("%s /c Explorer /e, '%s'", Ipaper:::cmd_wsl, path)
    )
    expect_identical(command, expected)
  }

  path <- file.path(tempdir(), "目录 空格 ' test")
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))
  fun <- mock_compat(dir.show,
    OS_type = function() "mac",
    check_path = identity,
    shell = function(cmd) command <<- cmd
  )
  file <- file.path(path, "file.xlsx")
  fun(file)
  expect_identical(command, sprintf("open %s", shQuote(dirname(file))))
})

test_that("fonts match filenames and families without shifting aliases", {
  local_mocked_bindings(
    font_files = function() data.frame(
      path = rep("/fonts", 4),
      file = c("Arial Bold.ttf", "MSYH.ttc", "Arial.ttf", "Times.ttc"),
      family = c("Arial", "Microsoft YaHei", "Arial", "Times"),
      face = c("Bold", "Regular", "Regular", "Regular")
    ),
    .package = "sysfonts"
  )
  fonts <- c(absent = "missing", Times = "Times", YH = "msyh",
    Arial = "Arial", Alias = "Microsoft YaHei"
  )
  expect_equal(Ipaper:::guess_font_path(fonts), c(
    Times = "/fonts/Times.ttc", YH = "/fonts/MSYH.ttc",
    Arial = "/fonts/Arial.ttf", Alias = "/fonts/MSYH.ttc"
  ))
  expect_length(Ipaper:::guess_font_path(c(absent = "missing")), 0L)

  registered <- NULL
  fun <- mock_compat(Ipaper:::add_fonts,
    guess_font_path = function(fonts) c(YH = "/fonts/MSYH.ttc"),
    font_add = function(family, path) registered <<- c(family, path)
  )
  fun(fonts)
  expect_identical(unname(registered), c("YH", "/fonts/MSYH.ttc"))
})

test_that("getwd_clip handles Windows and Unix clipboard paths", {
  for (windows in c(TRUE, FALSE)) {
    input <- if (windows) "C:\\目录 空格\\data" else "/tmp/目录 空格/a\\b"
    expected <- if (windows) "C:/目录 空格/data" else input
    copied <- NULL
    fun <- mock_compat(getwd_clip,
      is_win = function() windows,
      read_clip = function() input,
      write_clip = function(path) copied <<- path
    )
    expect_identical(fun(), expected)
    expect_identical(copied, expected)
  }
})

test_that("char2script uses the cross-platform clipboard helper", {
  for (interactive in c(TRUE, FALSE)) {
    copied <- NULL
    fun <- mock_compat(char2script,
      interactive = function() interactive,
      write_clip2 = function(script) copied <<- script
    )
    expect_identical(fun(c("ID", "NAME"), verbose = FALSE), 'c("ID", "NAME")')
    expect_identical(copied, if (interactive) 'c("ID", "NAME")' else NULL)
  }
})

test_that("gc_linux only invokes cleanup on Linux", {
  for (linux in c(FALSE, TRUE)) {
    command <- NULL
    fun <- mock_compat(gc_linux,
      is_linux = function() linux,
      system = function(cmd, ignore.stderr) {
        expect_true(ignore.stderr)
        command <<- cmd
      }
    )
    fun()
    expect_identical(command, if (linux) "rm /dev/shm -r" else NULL)
  }
})
