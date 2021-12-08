test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})

x <- list(a = 1:10, beta = exp(-3:3), logic = c(TRUE,FALSE,FALSE,TRUE))
r1 = llply(x, mean, .progress = "text")
r2 = llply(x, ~mean(.x), .progress = TRUE)
expect_equal(r1, r2)

r1 = laply(x, mean, .progress = "none")
r2 = laply(x, ~mean(.x), .progress = FALSE)
expect_equal(r1, r2)
