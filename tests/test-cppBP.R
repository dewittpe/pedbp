# Testing the cppBP function
library(pedbp)

x <- tryCatch(pedbp:::cppBP(0.5, 0.5, 34, 0, NA, NA, 0.5, source = c("martin2022", "gemelli1990"), type = "percentile"), error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "'source' should have length 1"))

x <- tryCatch(pedbp:::cppBP(0.5, 0.5, 34, 0, NA, NA, 0.5, source = c("martin2022", "not-a-source"), type = "percentile"), error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "'source' should have length 1"))

x <- tryCatch(pedbp:::cppBP(0.5, 0.5, 34, 0, NA, NA, 0.5, source = c("not-a-source"), type = "percentile"), error = function(e) e)
stopifnot(identical(class(x), c("simpleError", "error", "condition")))
stopifnot(identical(x$message, "Unknown source"))

################################################################################
##                                End of file                                 ##
################################################################################
