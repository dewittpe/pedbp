library(pedbp)

stopifnot(class(lo2013) == "data.frame")

# expected names:
stopifnot(all(c("age", "male") %in% names(lo2013)))

# testing charateristics of the data set
stopifnot( dim(lo2013) == c(30, 8))

stopifnot(min(lo2013$age) ==  3 * 12)
stopifnot(max(lo2013$age) == 17 * 12)
stopifnot(all(lo2013$male %in% c(0L, 1L)))
