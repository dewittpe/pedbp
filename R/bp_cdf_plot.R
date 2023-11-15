#' Plot the CDF for Blood Pressure
#'
#' Plot the CDF for blood pressure given age, sex, and height.
#'
#' @param age numeric age, in months
#' @param male integer value, 1 = male, 0 = female, indicating sex of the
#' patient
#' @param height numeric, in centimeters, can be missing.  This is the length
#' for patients under three years of age
#' @param height_percentile default height percentile to use if \code{height} is
#' missing.
#' @param sbp,dbp observed values to plot on the CDF
#' @param ... not currently used
#'
#' @return a \code{ggplot2} graphic showing the CDF for diastolic and systolic
#' blood pressures with vertical and horizontal lines highlight the percentile
#' for the given inputs.
#'
#' @examples
#' bp_cdf(age = 96, male = 1, sbp = 103, dbp = 55)
#'
#' @export
bp_cdf <- function(age, male, height = NA, height_percentile = 0.50, sbp = NA, dbp = NA, ...) {
  stopifnot(length(sbp) == 1L)
  stopifnot(length(dbp) == 1L)
  ps <- seq(0.001, 0.999, length = 100)
  bp <- q_bp(p_sbp = ps, p_dbp = ps, age = age, male = male, height = height, height_percentile = height_percentile)

  d <- data.frame(p = rep(ps, 2), bp = gl(n = 2, k = length(ps), labels = c('Systolic', 'Diastolic')), mmHg = c(bp$sbp, bp$dbp))

  obp <- p_bp(q_sbp = sbp, q_dbp = dbp, age = age, male = male, height = height, height_percentile = height_percentile)
  od <- data.frame(  mmHg = c(sbp, dbp)
                   , bp   = gl(n = 2, k = length(sbp), labels = c('Systolic', 'Diastolic'))
                   , p    = c(obp$sbp_percentile, obp$dbp_percentile)
                   )
  dseg <- data.frame(
                       bp = gl(n = 2, k = 2, labels = c('Systolic', 'Diastolic'))
                     , p    = c(obp$sbp_percentile, obp$sbp_percentile, obp$dbp_percentile, obp$dbp_percentile)
                     , pend = c(obp$sbp_percentile, -Inf,               obp$dbp_percentile, -Inf)
                     , mmHg = c(-Inf,               sbp,                -Inf,               dbp)
                     , mmHgend = c(sbp,             sbp,                dbp,                dbp)
  )

  ggplot2::ggplot(d) +
    ggplot2::theme_bw() +
    eval(substitute(ggplot2::aes(x = X, y = Y, linetype = LT), list(X = as.name("mmHg"), Y = as.name("p"), LT = as.name("bp")))) +
    ggplot2::geom_line() +
    ggplot2::scale_y_continuous(name = "Percentile", labels = scales::label_percent(suffix = "th")) +
    ggplot2::scale_linetype(guide = ggplot2::guide_legend(reverse = TRUE)) +
    ggplot2::theme(legend.position = "bottom"
                   , legend.title = ggplot2::element_blank()
    ) +
    ggplot2::geom_point(data = od) +
    ggplot2::geom_segment(data = dseg, mapping = ggplot2::aes_string(xend = "mmHgend", yend = "pend"), alpha = 0.5)

}

