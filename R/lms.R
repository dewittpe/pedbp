#' Distribution, Quantile, and Zscores by LMS values
#'
#' Non-exported functions ...
#'
#' @param x quantile or percentile value
#' @param l,m,s the lms values
#' @param ... pass through

zlms <- function(x, l, m, s, ...) {
  # stopifnot(all(s >= 0))
  # print(c(l, m, s))
  stopifnot(length(x) == 1L,
            length(l) == 1L,
            length(m) == 1L,
            length(s) == 1L)

  if (isTRUE(all.equal(0.0, l))) {
    z <- log( x / m) / s
  } else {
    z <- ( ((x / m) ^ l) - 1 ) / ( l * s)
  }
  z
}

plms <- function(x, l, m, s, ...) {
  stopifnot(length(x) == 1L,
            length(l) == 1L,
            length(m) == 1L,
            length(s) == 1L)
  z <- zlms(x, l, m, s)
  stats::pnorm(z, mean = 0, sd = 1)
}

qlms <- function(x, l, m, s, ...) {
  # stopifnot(all(s >= 0))
  stopifnot(length(x) == 1L,
            length(l) == 1L,
            length(m) == 1L,
            length(s) == 1L)

  z <- stats::qnorm(x, mean = 0, sd = 1)

  if (isTRUE(all.equal(0.0, l))) {
    rtn <- m * exp(s * z)
  } else {
    rtn <- m * (1 + l * s * z) ^ (1 / l)
  }
  rtn
}
