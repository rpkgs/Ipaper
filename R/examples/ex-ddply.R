dt <- data.table(x = 1:10, y = 1:5)
dt_ddply(dt, .(y), ~.[which.max(x)])
dt_ddply(dt, .(y), ~ top_n(., 1, x))
