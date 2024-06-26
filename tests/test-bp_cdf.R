library(pedbp)

################################################################################
x <- bp_cdf(age = 96, male = 1, sbp = 103, dbp = 55)
stopifnot(identical(class(x), c("gg", "ggplot")))

x <- p_bp(q_sbp = rep(100, 2),
          q_dbp = rep( 60, 2),
          age   = rep(35.75, 2),
          male  = c(0, 0),
          height = c(NA, 100))
x <- bp_cdf(x)
stopifnot(identical(class(x), "list"))
stopifnot(identical(length(x), 2L))
stopifnot(identical(class(x[[1]]), c("gg", "ggplot")))
stopifnot(identical(class(x[[2]]), c("gg", "ggplot")))


x <- q_bp(p_sbp = 0.85, p_dbp = 0.95,
          age = 29.2, male = 0, height_percentile = 0.95,
          source = "flynn2017")
x <- bp_cdf(x)
stopifnot(identical(class(x), "list"))
stopifnot(identical(length(x), 1L))
stopifnot(identical(class(x[[1]]), c("gg", "ggplot")))

################################################################################
#                                 End of File                                  #
################################################################################
