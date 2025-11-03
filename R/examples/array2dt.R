library(Ipaper)

arr <- array(1:6,
  dim = c(2, 3),
  dimnames = list(
    site = c("A", "B"),
    date = c("d1", "d2", "d3")
    # var = c("v1", "v2", "v3", "v4")
  )
)

# array -> dt
dt <- array2dt(arr, dimnames(arr))
print(dt)

# dt -> array
arr2 <- dt2array(dt)
all.equal(arr, arr2)
