#' Estimate BP from percentile for Age, Sex and Height
#'
#'
#' @param sbp_percentile numeric between 0, 1
#' @param dbp_percentile numeric between 0, 1
#' @param age age in months
#' @param male integer value, 1 = male, 0 = female
#' @param height numeric, in centimeters, can be missing.
#' @param height_percentile default height percentile to use if \code{height} is
#' missing.
#' @param ... not currently used
#'
#' @examples
#' get_bp(sbp_percentile = 0.9, dbp = 0.60, age = 8, male = 0)
#' @export
get_bp <- function(sbp_percentile = NA, dbp_percentile = NA, age, male, height = NA, height_percentile = 0.50, ...) {
  stopifnot(length(age) == 1L)
  stopifnot(length(male) == 1L)
  stopifnot(all(age >=0) & all(age <= 18 * 12))
  stopifnot(all(male %in% c(0L, 1L)))
  stopifnot(all(stats::na.omit(height > 0)))
  stopifnot(0 < height_percentile & height_percentile < 1)
  stopifnot(!is.na(sbp_percentile) & !is.na(dbp_percentile))
  if (!is.na(sbp_percentile)) {
    stopifnot(0 < sbp_percentile & sbp_percentile < 1)
  }
  if (!is.na(dbp_percentile)) {
    stopifnot(0 < dbp_percentile & dbp_percentile < 1)
  }

  if (!is.na(height)) {
    height_percentile <- get_height_percentile(age, male, height)
  }

  e <- new.env()
  utils::data(list = "bp_parameters", package = "pedbp", envir = e)
  d <- e$bp_parameters[e$bp_parameters$male == male, ]

  if (age < 12) {
    d <- d[d$age < age, ]
    d <- d[which.max(age), ]
    height_percentile <- NA_real_
  } else if (is.na(height) & age >= 36) {
    d <- d[is.na(d$height_percentile), ]
    d <- d[d$age <= age, ]
    d <- d[d$age == max(d$age), ]
    height_percentile <- NA_real_
  } else {
    d <- d[d$age <= age, ]
    d <- d[d$age == max(d$age), ]
    d <- d[d$height_percentile <= height_percentile * 100, ]
    d <- d[d$height_percentile == max(d$height_percentile), ]
  }

  rtn <-
    list(  sbp = stats::qnorm(sbp_percentile, mean = d$sbp_mean, sd = d$sbp_sd)
         , dbp = stats::qnorm(dbp_percentile, mean = d$dbp_mean, sd = d$dbp_sd)
         , height_percentile = height_percentile)
  rtn
}

