library(pedbp)

stopifnot(identical(class(lo2013), "data.frame"))

# expected names:
stopifnot(identical(names(lo2013), c("age", "male", "sbp_mean", "sbp_sd", "dbp_mean", "dbp_sd")))

# testing charateristics of the data set
stopifnot(identical(dim(lo2013),  c(30L, 6L)))

stopifnot(isTRUE(min(lo2013$age) ==  3 * 12))
stopifnot(isTRUE(max(lo2013$age) == 17 * 12))
stopifnot(isTRUE(all(lo2013$male %in% c(0L, 1L))))

################################################################################
##                                End of file                                 ##
################################################################################
