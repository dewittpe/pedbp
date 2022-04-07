library(pedbp)

# testing charateristics of the data set
stopifnot( dim(bp_age_height) == c(952, 8))

stopifnot(min(bp_age_height$age_years) == 1)
stopifnot(max(bp_age_height$age_years) == 17)
stopifnot(all(bp_age_height$male %in% c(0, 1)))
