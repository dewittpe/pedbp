#' Estimate Normal Distribution Given Set of Quantile Values
#'
#' With at least two quantile values find the mean and standard deviation of a
#' normal distribution to match up with empirical values provided.
#'
#' For X ~ N(mu, sigma), Pr[X <= q] = p
#'
#' Given the set of quantiles and probabilities, \code{est_norm} uses
#' \code{\link[stats]{optim}} to find the preferable mean and standard deviation
#' of a normal distribution to fit the provided quantiles.
#'
#' Use the \code{weight} argument to emphasize which, if any, of the provided
#' quantiles needs to be approximated closer than others.  By default all the
#' quantiles are weighted equally.
#'
#' @param q quantile values.
#' @param p probabilities corresponding to the \code{q} quantiles.
#' @param weights relative weight of each quantile.  The higher the weight the
#' better the approximated distribution will be at fitting that quantile.
#' @param ... passed to \code{\link[stats]{optim}}.
#'
#' @examples
#'
#' # Example 1
#' q <- c(-1.92, 0.1, 1.89) * 1.8 + 3.14
#' p <- c(0.025, 0.50, 0.975)
#'
#' x <- est_norm(q, p)
#' x
#'
#' plot(x)
#'
#' # Example 2 -- build with quantiles that are easy to see unlikely to be from
#' # a Normal distribuiton
#' q <- c(-1.92, 0.05, 0.1, 1.89) * 1.8 + 3.14
#' p <- c(0.025, 0.40, 0.50, 0.975)
#'
#' # with equal weights
#' x <- est_norm(q, p)
#' x
#' plot(x)
#'
#' # weight to ignore one of the middle value and make sure to hit the other
#' x <- est_norm(q, p, weights = c(1, 2, 0, 1))
#' x
#' plot(x)
#'
#' # equal weight the middle, more than the tails
#' x <- est_norm(q, p, weights = c(1, 2, 2, 1))
#' x
#' plot(x)
#'
#' @export
est_norm <- function(q, p, weights = rep(1, length(p)), ...) {
  stopifnot(length(q) > 1L & length(p) > 1L)
  stopifnot(length(q) == length(p))
  stopifnot(all(p > 0) & all(p < 1))
  stopifnot(is.numeric(q))

  # get an initial estimate for the mean and standard deviation
  # mean estimated by the median
  # standard deviation by ratio of range of value quantiles
  fit <- stats::lm(q ~ p)
  mean_est <- unname(stats::predict(fit, newdata = list(p = 0.5)))
  sd_est   <- stats::predict(fit, newdata = list(p = c(0.025, 0.975)))
  sd_est   <- unname(diff(sd_est) / diff(stats::qnorm(c(0.025, 0.975))))

  # define a function to minimize via stats::optim
  sum_squared_resid <- function(x) {
    if (x[2] < 0) {
      x[2] <- 0.01
    }
    res <- stats::pnorm(q = q, mean = x[1], sd = x[2]) - p
    res <- res * weights
    sum(res**2)
  }

  # call optim
  optim <- stats::optim(par = c(mean_est, sd_est),
                        fn  = sum_squared_resid,
                        ...)

  rtn <- list(
    par = c("mean" = optim$par[1], "sd" = optim$par[2]),
    qp   = cbind(q, p),
    weights = weights,
    call = match.call(),
    optim = optim)

  class(rtn) <- "pedbp_est_norm"

  rtn
}

#' @export
print.pedbp_est_norm <- function(x, ...) {
  print(x$par)
  invisible(x)
}

#' @export
plot.pedbp_est_norm <- function(x, y, ...) {
  plot(x = x$qp[, "q"], y = x$qp[, "p"],
       xlab = "Quantile", ylab = "Probability", ...)
  m = x$par[1]
  s = x$par[2]
  foo <- function(x) {
    stats::pnorm(x, mean = m, sd = s)
  }
  graphics::curve(foo, add = TRUE)
}
