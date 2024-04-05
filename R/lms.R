#' Distribution, Quantile, and Z-scores by LMS values
#'
#' Functions for getting estimated distribution, quantile, and standard scores
#' (z-scores) given LMS parameters.
#'
#' The parameters need to be either length 1 or of equal length.
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
#' \neq 0 \\ \frac{\log\left(\frac{X}{M}\right)}{S} & L = 0. \end{cases}}{
#' ( (X/M)^L - 1) / (LS) for L != 0; log(X/M) / S for L = 0.}
#'
#' @param x quantile or probability value
#' @param l,m,s the lms values
#' @param ... pass through
#'
#' @return a numeric vector
#'
#' @references Cole, Timothy J., and Pamela J. Green. "Smoothing reference
#' centile curves: the LMS method and penalized likelihood." Statistics in
#' medicine 11.10 (1992): 1305-1319.
#'
#' @examples
#'
#' l <- -0.1600954
#' m <-  9.476500305
#' s <-  0.11218624
#'
#' # the 5th quantile:
#' qlms(x = 0.05, l = l, m = m, s = s)
#'
#' # What percentile is the value 8.2?
#' plms(x = 8.2, l = l, m = m, s = s)
#'
#' # What is the standard score for the value 8.2
#' zlms(x = 8.2, l = l, m = m, s = s)
#'
#' all.equal(
#'   zlms(x = 8.2, l = l, m = m, s = s)
#'   ,
#'   qnorm(plms(x = 8.2, l = l, m = m, s = s))
#' )
#'
#' # get all the quantiles form the 5th through 95th for a set of LMS parameters
#' ps <- seq(0.05, 0.95, by = 0.05)
#' qs <- qlms(x = ps, l = l, m = m, s = s)
#' all.equal(plms(qs, l, m, s), ps)
#' all.equal(zlms(x = qs, l = l, m = m, s = s), qnorm(ps))
#'
#' @name distribution-quantile-zscores-by-lms
NULL

#' @rdname distribution-quantile-zscores-by-lms
#' @export
zlms <- function(x, l, m, s, ...) {
  params <- lms_arg_check(x, l, m, s)

  rtn <-
    apply(params, MARGIN = 1, function(xx) {
      if (isTRUE(all.equal(0.0, unname(xx["l"])))) {
        z <- log( xx["x"] / xx["m"]) / xx["s"]
      } else {
        z <- ( ((xx["x"] / xx["m"]) ^ xx["l"]) - 1 ) / ( xx["l"] * xx["s"])
      }
      z
    })
  unname(rtn)
}

#' @rdname distribution-quantile-zscores-by-lms
#' @export
plms <- function(x, l, m, s, ...) {
  z <- zlms(x, l, m, s)
  stats::pnorm(z, mean = 0, sd = 1)
}

#' @rdname distribution-quantile-zscores-by-lms
#' @export
qlms <- function(x, l, m, s, ...) {

  params <- lms_arg_check(x, l, m, s)
  params <- cbind(params, z = stats::qnorm(params[, "x"], mean = 0, sd = 1))

  rtn <-
    apply(params, MARGIN = 1, function(xx) {

      if (isTRUE(all.equal(0.0, unname(xx["l"])))) {
        rtn <- xx["m"] * exp(xx["s"] * xx["z"])
      } else {
        rtn <- xx["m"] * (1 + xx["l"] * xx["s"] * xx["z"]) ^ (1 / xx["l"])
      }
      rtn
              })
  unname(rtn)
}

lms_arg_check <- function(x, l, m, s) {
  lngths <- lengths(list(x, l, m, s))
  max_length <- max(lngths)

  stopifnot(all(lengths(list(x, l, m, s)) %in% c(1, max(lengths(list(x, l, m, s))))))

  if (max_length > 1 & any(lngths == 1L)) {
    if (length(x) == 1L) {
      x <- rep(x, max_length)
    }

    if (length(l) == 1L) {
      l <- rep(l, max_length)
    }

    if (length(m) == 1L) {
      m <- rep(m, max_length)
    }

    if (length(s) == 1) {
      s <- rep(s, max_length)
    }
  }

  cbind(x = x, l = l , m = m, s = s)
}

