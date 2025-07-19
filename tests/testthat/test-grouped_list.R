test_that("grouped_list", {
  r <- mtcars %>%
    as.data.table() %>%
    group_by(cyl) %>%
    group_map2(~ head(.x, 2L) %>% as.data.table(),
      .progress = TRUE, "data"
    )
  ## 需要测试tibble转为data.table是否存在较大的开销
  export_fst(r, "test.fst")
  r2 <- import_fst("test.fst")
  expect_equal(rbindlist(r$data), rbindlist(r2$data))

  file.remove("test.fst")
  file.remove("test_group.csv")
})
