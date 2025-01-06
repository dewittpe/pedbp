library(pedbp)
################################################################################
# verify errors if inputs are not as expected
x <- tryCatch(bp_chart(male = c(1, 1, 0)), error = function(e) e)
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "length(male) %in% c(1L, 2L) is not TRUE"))

x <- tryCatch(bp_chart(male = numeric(0)), error = function(e) e)
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "length(male) %in% c(1L, 2L) is not TRUE"))

x <- tryCatch(bp_chart(male = -1), error = function(e) e)
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "male == 1 | male == 0 is not TRUE"))

x <- tryCatch(bp_chart(height = numeric(0)), error = function(e) e)
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "length(height) == 1 is not TRUE"))

x <- tryCatch(bp_chart(height = numeric(2)), error = function(e) e)
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "length(height) == 1 is not TRUE"))

x <- tryCatch(bp_chart(height_percentile = numeric(0)), error = function(e) e)
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "length(height_percentile) == 1 is not TRUE"))

x <- tryCatch(bp_chart(height_percentile = numeric(2)), error = function(e) e)
stopifnot(inherits(x, "error"))
stopifnot(identical(x$message, "length(height_percentile) == 1 is not TRUE"))

x <- tryCatch(bp_chart(source = numeric(2)), error = function(e) e)
stopifnot(inherits(x, "error"))

x <- tryCatch(bp_chart(source = "not-a-source"), error = function(e) e)
stopifnot(inherits(x, "error"))

x <- tryCatch(bp_chart(bp = c("not-a-bp")), error = function(e) e)
stopifnot(inherits(x, "error"))

################################################################################
# verify defult is a ggplot
x <- bp_chart()
stopifnot(identical(class(x), c("gg", "ggplot")))

################################################################################
#                                 End of File                                  #
################################################################################
