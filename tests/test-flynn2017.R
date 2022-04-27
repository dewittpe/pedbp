library(pedbp)

# testing charateristics of the data set
stopifnot( dim(flynn2017) == c(952, 8))

stopifnot(min(flynn2017$age_years) == 1)
stopifnot(max(flynn2017$age_years) == 17)
stopifnot(all(flynn2017$male %in% c(0, 1)))
