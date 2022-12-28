library(ggplot2)

test_that("upper_envelope works", {
  y = rnorm(1000)
  x = seq_along(y)

  expect_true({
    r = upper_envelope(x, y, nchunk = 20)

    ggplot(r, aes(x, mid)) +
      geom_ribbon(aes(ymin = lower, ymax = upper, fill = interval), alpha = 0.5) +
      geom_line(linewidth = 1.5, color = "blue")

    is.data.table(r) && nrow(r) == 80
  })

  expect_true({
    r2 = group_apply(x, y, nchunk = 20)
    is.data.table(r2) && nrow(r2) == 20
  })
})
