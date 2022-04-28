library(pedbp)

# expect an error if q and/or p have length 1
test <- tryCatch(est_norm(q = c(1), p = c(0.1)), error = function(e) e)
stopifnot(inherits(test,"simpleError"))
stopifnot(test$message == "length(q) > 1L & length(p) > 1L is not TRUE")


# expect an error when q and p are of different length
test <- tryCatch(est_norm(q = c(1, 2), p = c(0.1, 0.2, 0.3)), error = function(e) e)
stopifnot(inherits(test,"simpleError"))
stopifnot(test$message == "length(q) == length(p) is not TRUE")

# quick check of results
m <- pi
s <- (1 + sqrt(5)) / 2
ps <- c(0.1988159, 0.5340165, 0.8743177, 0.7812)
qs <- qnorm(ps, mean = m, sd = s)
out <- est_norm(qs, ps)

stopifnot(abs(m - out$par[1]) < 0.0001)
stopifnot(abs(s - out$par[2]) < 0.0001)
