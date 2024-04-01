#' Plot the CDF for Blood Pressure
#'
#' Plot the CDF for blood pressure given age, sex, and height.
#'
#'
#' @param ... not currently used
#'
#' @return
#' When passing in a \code{pedbp_bp} object, the return is a list of
#' \code{ggplot} objects.  The length of the list is equal to the length of the
#' number of quantiles or percentiles within the \code{pedbp_bp} object.
#'
#' When using the default method the return is just
#' a \code{ggplot} object showing the CDF for diastolic and systolic
#' blood pressures with vertical and horizontal lines highlight the percentile
#' for the given inputs.
#'
#' @examples
#'
#' # Explicity defining the inputs
#' bp_cdf(age = 96, male = 1, sbp = 103, dbp = 55)
#'
#' # Plotting two cdfs from a call to p_bp
#' x <- p_bp(q_sbp = rep(100, 2),
#'           q_dbp = rep( 60, 2),
#'           age   = rep(35.75, 2),
#'           male  = c(0, 0),
#'           height = c(NA, 100))
#'
#' bp_cdf(x)
#'
#' # Plotting a cdf from a call to q_bp
#' x <- q_bp(p_sbp = 0.85, p_dbp = 0.95,
#'           age = 29.2, male = 0, height_percentile = 0.95,
#'           source = "flynn2017")
#'
#' bp_cdf(x)
#'
#' @name bp_cdf
NULL

#' @rdname bp_cdf
#' @export
bp_cdf <- function(...) {
  UseMethod("bp_cdf")
}

#' @rdname bp_cdf
#' @export
#' @param x a \code{pedbp_bp} object created by \code{\link{q_bp}} or
#' \code{\link{p_bp}}.
bp_cdf.pedbp_bp <- function(x, ...) {
  NextMethod("bp_cdf")
}

#' @rdname bp_cdf
#' @export
bp_cdf.pedbp_p_bp <- function(x, ...) {
  params <- attr(x, "bp_params")
  Map(bp_cdf
      , age = attr(x, "bp_params")[["age"]]
      , male = attr(x, "bp_params")[["male"]]
      , height_percentile = attr(x, "bp_params")[["height_percentile"]] / 100
      , source = attr(x, "bp_params")[["source"]]
      , sbp = stats::qnorm(x$sbp_percentile, mean = attr(x, "bp_params")$sbp_mean, sd = attr(x, "bp_params")$sbp_sd)
      , dbp = stats::qnorm(x$dbp_percentile, mean = attr(x, "bp_params")$dbp_mean, sd = attr(x, "bp_params")$dbp_sd)
  )
}

#' @rdname bp_cdf
#' @export
bp_cdf.pedbp_q_bp <- function(x, ...) {
  params <- attr(x, "bp_params")
  Map(bp_cdf
      , age = attr(x, "bp_params")[["age"]]
      , male = attr(x, "bp_params")[["male"]]
      , height_percentile = attr(x, "bp_params")[["height_percentile"]] / 100
      , source = attr(x, "bp_params")[["source"]]
      , sbp = x$sbp
      , dbp = x$dbp
  )
}

#' @rdname bp_cdf
#' @export
#' @inheritParams bp_distribution
#' @param sbp the observed systolic blood pressure
#' @param dbp the observed diastolic blood pressure
bp_cdf.default <- function(age, male, height = NA, height_percentile = 0.50, source = getOption("pedbp_bp_source", "martin2022"), sbp = NA, dbp = NA, ...) {

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

  bpcdfplot(od, dseg, params)
}

bpcdfplot <- function(od, dseg, params) {
  ggplot2::ggplot() +
    ggplot2::xlim(
                  min(c(od$mmHg, stats::qnorm(1e-4, mean = params$dbp_mean, sd = params$dbp_sd))),
                  max(c(od$mmHg, stats::qnorm(1 - 1e-4, mean = params$sbp_mean, sd = params$sbp_sd)))
                  ) +
    ggplot2::xlab("mmHg") +
    ggplot2::scale_y_continuous(name = "Percentile", labels = scales::label_percent(suffix = "th")) +
    ggplot2::geom_function(mapping = ggplot2::aes(linetype = "Systolic"), fun = stats::pnorm, args = list(mean = params$sbp_mean, sd = params$sbp_sd), inherit.aes = FALSE) +
    ggplot2::geom_function(mapping = ggplot2::aes(linetype = "Diastolic"), fun = stats::pnorm, args = list(mean = params$dbp_mean, sd = params$dbp_sd), inherit.aes = FALSE) +
    eval(substitute(ggplot2::geom_point(data = od, mapping = ggplot2::aes(x = MMHG, y = P)),
                    list(MMHG = as.name("mmHg"), P = as.name("p")))) +
    eval(substitute(ggplot2::geom_segment(data = dseg, mapping = ggplot2::aes(x = MMHG, y = P, xend = MMHGEND, yend = PEND, linetype = BP)),
                    list(MMHG = as.name("mmHg"), P = as.name("p"), MMHGEND = as.name("mmHgend"), PEND = as.name("pend"), BP = as.name("bp")))) +
    ggplot2::scale_linetype(guide = ggplot2::guide_legend(reverse = TRUE))  +
    ggplot2::theme(legend.position = "bottom", legend.title = ggplot2::element_blank())
}

