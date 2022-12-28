test_that("stat_quantile2 works", {

  x = 1:10
  r_sd = stat_sd(x)
  expect_equal(names(r_sd), c("y", "ymin", "ymax", "ymedian", "sd", "label"))

  probs = c(.25, .75)
  expect_equal(
    unlist(stat_quantile2(x, probs))[-1],
    box_qtl(x, probs)
  )

  expect_equal(
    box_qtl(x, probs), 
    quantile_envelope(x, probs[1])
  )  
})
