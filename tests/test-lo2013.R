library(pedbp)

# testing charateristics of the data set
stopifnot( dim(lo2013) == c(30, 14))

stopifnot(min(lo2013$age) == 3)
stopifnot(max(lo2013$age) == 17)
stopifnot(all(lo2013$sex %in% c('male', 'female')))
