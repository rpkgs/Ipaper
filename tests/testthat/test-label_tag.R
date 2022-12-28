test_that("label_tag works", {
  expect_equal(
    label_tag(1:5),
    expression(
      bold("(" * "a" * ")" ~ 1L), bold("(" * "b" * ")" ~
        2L), bold("(" * "c" * ")" ~ 3L), bold("(" * "d" * ")" ~ 4L),
      bold("(" * "e" * ")" ~ 5L)
    )
  )
  
  expect_equal(
    char2script(1:5, verbose = FALSE), 
    "c(\"1\", \"2\", \"3\", \"4\", \"5\")")
})
