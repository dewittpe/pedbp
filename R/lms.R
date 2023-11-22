#' Distribution, Quantile, and Z-scores by LMS values
#'
#' Functions for getting estimated distribution, quantile, and standard scores
#' (z-scores) given LMS parameters.
#'
#'
#' L is the power in the Box-Cox transformation, M the median, and S a
#' generalized coefficient of variation. For a given standard score (z-score),
#' Z, the value X of interest is
#'
#' \deqn{ X = \begin{cases} M (1 + LSZ)^{1/L} & L \neq 0 \\ M \exp(SZ) & L = 0.
#' \end{cases} }{M (1 + LSZ)^(1/L) for L != 0; M exp(SZ) for L = 0.}
#'
#' To get the z-score for a value X:
#'
#' \deqn{Z = \begin{cases} \frac{ \left(\frac{X}{M}\right)^{L} - 1 }{LS} & L
#' \neq 0 \\ \frac{\log\left(\frac{X}{M}}{S} & L = 0.}{ ( (X/M)^L - 1) / (LS)
#' for L != 0; log(X/M) / S for L = 0.}
#'
#' @param x quantile or percentile value
#' @param l,m,s the lms values
#' @param ... pass through
#'
#' @references Cole, Timothy J., and Pamela J. Green. "Smoothing reference
#' centile curves: the LMS method and penalized likelihood." Statistics in
#' medicine 11.10 (1992): 1305-1319.
#'
#' @name distribution-quantile-zscores-by-lms
NULL

#' @rdname distribution-quantile-zscores-by-lms
#' @export
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

#' @rdname distribution-quantile-zscores-by-lms
#' @export
plms <- function(x, l, m, s, ...) {
  stopifnot(length(x) == 1L,
            length(l) == 1L,
            length(m) == 1L,
            length(s) == 1L)
  z <- zlms(x, l, m, s)
  stats::pnorm(z, mean = 0, sd = 1)
}

#' @rdname distribution-quantile-zscores-by-lms
#' @export
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
