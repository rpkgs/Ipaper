rio_test_data <- function() {
  data.frame(
    id = 1:4,
    value = c(NA_real_, NaN, Inf, -1.5),
    text = c("中文", "", NA_character_, "a b"),
    flag = c(TRUE, FALSE, NA, TRUE),
    group = factor(c("a", "b", NA, "a")),
    date = as.Date(c("2024-01-01", NA, "2024-02-29", "2025-01-01")),
    time = as.POSIXct(c(0, NA, 3600, 86400), origin = "1970-01-01", tz = "UTC")
  )
}

test_that("qs and qs2 imports read qs2 files", {
  x <- list(data = head(iris), values = 1:3)
  files <- vapply(c("qs", "qs2", "QS2"), function(ext) {
    tempfile(fileext = paste0(".", ext))
  }, character(1))
  on.exit(unlink(files))

  for (f in files) {
    qs2::qs_save(x, f, nthreads = 1)
    expect_identical(import(f, nthreads = 1), x)
  }
})

test_that("qdata import and export match qs2", {
  x <- list(data = head(iris), values = 1:3)
  f <- tempfile(fileext = ".qdata")
  on.exit(unlink(f))

  export(x, f, nthreads = 1)
  expect_identical(qs2::qd_read(f, nthreads = 1), x)
  expect_identical(import(f, nthreads = 1), x)

  qs2::qd_save(x, f, nthreads = 1)
  expect_identical(import(f, nthreads = 1), x)
})

test_that("qs2 and qdata round-trips preserve types and match official readers", {
  x <- list(data = rio_test_data(), matrix = matrix(1:6, 2), empty = NULL)
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))

  for (ext in c("qs", "qs2", "qdata", "QS2", "QDATA")) {
    read <- if (tolower(ext) == "qdata") qs2::qd_read else qs2::qs_read
    write <- if (tolower(ext) == "qdata") qs2::qd_save else qs2::qs_save
    f <- file.path(path, paste0("中文 空格 ' .", ext))
    for (n in c(1, 6)) {
      export(x, f, nthreads = n, compress_level = 1)
      expect_identical(import(f, nthreads = n), x)
      expect_identical(read(f, nthreads = n), x)
      write(x, f, nthreads = n)
      expect_identical(import(f, nthreads = n), x)
    }
  }
})

test_that("arrow and feather round-trips preserve column types", {
  skip_if_not_installed("arrow")
  x <- rio_test_data()
  path <- tempfile()
  dir.create(path)
  on.exit(unlink(path, recursive = TRUE))

  for (ext in c("arrow", "feather", "ARROW", "FEATHER")) {
    f <- file.path(path, paste0("中文 空格 ' .", ext))
    export(x, f, compression = "uncompressed")
    expect_identical(as.data.frame(import(f)), x)
    expect_identical(as.data.frame(arrow::read_feather(f)), x)
    expect_s3_class(import(f, as_data_frame = FALSE), "Table")

    arrow::write_feather(x, f)
    expect_identical(as.data.frame(import(f)), x)
  }
})
