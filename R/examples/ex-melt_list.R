# data.frame
df <- data.frame(year = 2010, day = 1:3, month = 1, site = "A")
l <- list(a = df, b = df)

melt_list(l, 'id') %>% as_tibble()
melt_list(l, 'id' = 1) %>% as_tibble()
melt_list(set_names(l, NULL), 'id') %>% as_tibble()

# data.table
df <- data.table::data.table(year = 2010, day = 1:3, month = 1, site = "A")
l <- list(a = df, b = df)
melt_list(l, "id")
melt_list(l, id = c("a", "b"))
# int `id` is much more efficient
melt_list(l, id = c(1, 2))
