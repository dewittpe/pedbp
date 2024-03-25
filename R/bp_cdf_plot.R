#' Plot the CDF for Blood Pressure
#'
#' Plot the CDF for blood pressure given age, sex, and height.
#'
#' @inheritParams bp_distribution
#'
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
bp_cdf <- function(age, male, height = NA, height_percentile = 0.50, source = getOption("pedbp_bp_source", "martin2022"), sbp = NA, dbp = NA, ...) {

  stopifnot(length(age) == 1L)
  stopifnot(length(male) == 1L)
  stopifnot(length(height) == 1L)
  stopifnot(length(height_percentile) == 1L)
  stopifnot(length(source) == 1L)
  stopifnot(length(sbp) == 1L)
  stopifnot(length(dbp) == 1L)

  pbp <- p_bp(q_sbp = sbp, q_dbp = dbp, age = age, male = male, height = height, height_percentile = height_percentile, source = source)
  params <- attr(pbp, "bp_params")

  od <- data.frame(  mmHg = c(sbp, dbp)
                   , bp   = gl(n = 2, k = length(sbp), labels = c('Systolic', 'Diastolic'))
                   , p    = c(pbp$sbp_percentile, pbp$dbp_percentile)
                   )
  dseg <- data.frame(
                       bp = gl(n = 2, k = 2, labels = c('Systolic', 'Diastolic'))
                     , p    = c(pbp$sbp_percentile, pbp$sbp_percentile, pbp$dbp_percentile, pbp$dbp_percentile)
                     , pend = c(pbp$sbp_percentile, -Inf,               pbp$dbp_percentile, -Inf)
                     , mmHg = c(-Inf,               sbp,                -Inf,               dbp)
                     , mmHgend = c(sbp,             sbp,                dbp,                dbp)
  )

  ggplot2::ggplot() +
    ggplot2::xlim(
                  min(c(od$mmHg, stats::qnorm(1e-4, mean = params$dbp_mean, sd = params$dbp_sd))),
                  max(c(od$mmHg, stats::qnorm(1 - 1e-4, mean = params$sbp_mean, sd = params$sbp_sd)))
                  ) +
    ggplot2::xlab("mmHg") +
    ggplot2::scale_y_continuous(name = "Percentile", labels = scales::label_percent(suffix = "th")) +
    ggplot2::geom_function(mapping = ggplot2::aes(linetype = "Systolic"), fun = stats::pnorm, args = list(mean = params$sbp_mean, sd = params$sbp_sd), inherit.aes = FALSE) +
    ggplot2::geom_function(mapping = ggplot2::aes(linetype = "Diastolic"), fun = stats::pnorm, args = list(mean = params$dbp_mean, sd = params$dbp_sd), inherit.aes = FALSE) +
    ggplot2::geom_point(data = od, mapping = ggplot2::aes(x = mmHg, y = p)) +
    ggplot2::geom_segment(data = dseg, mapping = ggplot2::aes(x = mmHg, y = p, xend = mmHgend, yend = pend, linetype = bp)) +
    ggplot2::scale_linetype(guide = ggplot2::guide_legend(reverse = TRUE))  +
    ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank())
}

