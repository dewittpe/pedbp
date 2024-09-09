library(pedbp)

################################################################################
# expect an error if q and/or p have length 1
test <- tryCatch(est_norm(q = c(1), p = c(0.1)), error = function(e) e)
stopifnot(inherits(test,"error"))
stopifnot(identical(test$message, "length(q) > 1L & length(p) > 1L is not TRUE"))

test <- tryCatch(est_norm(q = c(1), p = c(0.1, 0.2)), error = function(e) e)
stopifnot(inherits(test,"error"))
stopifnot(identical(test$message, "length(q) > 1L & length(p) > 1L is not TRUE"))

test <- tryCatch(est_norm(q = c(1, 2), p = c(0.2)), error = function(e) e)
stopifnot(inherits(test,"error"))
stopifnot(identical(test$message, "length(q) > 1L & length(p) > 1L is not TRUE"))

test <- tryCatch(est_norm(q = c(1, 2), p = numeric(1)), error = function(e) e)
stopifnot(inherits(test,"error"))
stopifnot(identical(test$message, "length(q) > 1L & length(p) > 1L is not TRUE"))

test <- tryCatch(est_norm(q = c(1, 2)), error = function(e) e)
stopifnot(inherits(test,"error"))
stopifnot(identical(test$message, "argument \"p\" is missing, with no default"))

test <- tryCatch(est_norm(p = c(0.1, 0.22)), error = function(e) e)
stopifnot(inherits(test,"error"))
stopifnot(identical(test$message, "argument \"q\" is missing, with no default"))

################################################################################
# expect an error when q and p are of different length
test <- tryCatch(est_norm(q = c(1, 2), p = c(0.1, 0.2, 0.3)), error = function(e) e)
stopifnot(inherits(test,"error"))
stopifnot(identical(test$message, "length(q) == length(p) is not TRUE"))

################################################################################
# check that all( p > 0 & p < 1)
test <- tryCatch(est_norm(q = c(1, 2), p = numeric(2)), error = function(e) e)
stopifnot(inherits(test,"error"))
stopifnot(identical(test$message, "all(p > 0) & all(p < 1) is not TRUE"))

test <- tryCatch(est_norm(q = c(1, 2), p = c(0.1, 02)), error = function(e) e)
stopifnot(inherits(test,"error"))
stopifnot(identical(test$message, "all(p > 0) & all(p < 1) is not TRUE"))

################################################################################
# quick check of results
set.seed(42)
m <- pi
s <- (1 + sqrt(5)) / 2
ps <- c(0.1988159, 0.5340165, 0.8743177, 0.9812)
qs <- qnorm(ps, mean = m, sd = s)
out <- est_norm(qs, ps)

stopifnot(identical(names(out$par), c("mean", "sd")))
stopifnot(isTRUE(abs(m - out$par[1]) < 0.0001))
stopifnot(isTRUE(abs(s - out$par[2]) < 0.0001))

# also check that the printing method returns the object
out2 <- print(out)
stopifnot(identical(out2, out))

# the print method is identical to the print(x$par)
stopifnot(identical(capture.output(print(out)), capture.output(print.default(out$par))))

################################################################################
# what happens when a completely insane set of values is used to start?
#
# unsorted values will error
ps2 <- sample(ps)
qs2 <- sample(ps)

test <- tryCatch(est_norm(qs, ps2), error = function(e) e)
stopifnot(identical(test$message, "q and p are expected to be sorted in ascending order."))

test <- tryCatch(est_norm(qs2, ps), error = function(e) e)
stopifnot(identical(test$message, "q and p are expected to be sorted in ascending order."))

test <- tryCatch(est_norm(qs2, ps2), error = function(e) e)
stopifnot(identical(test$message, "q and p are expected to be sorted in ascending order."))

################################################################################
# test the plot -- this was okay on one machine, but fails on other machines
#g <- plot(out)
#
## when needed, update the saved plot that is tested against
## ggplot2::ggsave(g, file = "tests/plot.est_norm.png", width = 7, height = 7)
#
## build and save a graphic
#tmpfile <- tempfile(fileext = ".png")
#ggplot2::ggsave(g, file = tmpfile, width = 7, height = 7)
#
## read the graphics
#if (interactive()) {
#  expected_img <- png::readPNG("tests/plot.est_norm.png")
#} else {
#  expected_img <- png::readPNG("plot.est_norm.png")
#}
#new_img <- png::readPNG(tmpfile)
#
## compare the graphics
#stopifnot(identical(new_img, expected_img))

################################################################################
##                                End of file                                 ##
################################################################################
