library(pedbp)

stopifnot(class(flynn2017) == "data.frame")

# expected names:
stopifnot(all(c("age", "male", "sbp", "dbp", "height") %in% names(flynn2017)))

# testing charateristics of the data set
stopifnot( dim(flynn2017) == c(952, 7))

stopifnot(min(flynn2017$age) ==  1 * 12)
stopifnot(max(flynn2017$age) == 17 * 12)
stopifnot(all(flynn2017$male %in% c(0, 1)))
