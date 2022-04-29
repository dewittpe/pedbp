#' Estimate Pediatric Blood Pressure Percentiles
#'
#' Estimate the percentile for blood pressure and height for a pediatric patient
#' based on sex and age and height.  Sex and age are required, height is not.
#'
#' @param sbp Systolic blood pressure, mmHg
#' @param dbp Systolic blood pressure, mmHg
#' @param age age in months
#' @param male integer value, 1 = male, 0 = female
#' @param height numeric, in centimeters, can be missing.
#' @param height_percentile default height percentile to use if \code{height} is
#' missing.
#' @param ... not currently used
#'
#' @examples
#' get_bp_percentile(sbp = 100, dbp = 60, age = 8, male = 0)
#' @export
get_bp_percentile <- function(sbp, dbp, age, male, height = NA, height_percentile = 0.50, ...) {
  stopifnot(length(age) == 1L)
  stopifnot(length(male) == 1L)
  stopifnot(all(age >=0) & all(age < 19))
  stopifnot(all(male %in% c(0L, 1L)))
  stopifnot(all(stats::na.omit(height > 0)))
  stopifnot(0 < height_percentile & height_percentile < 1)

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
    list(  sbp_percentile = stats::pnorm(sbp, mean = d$sbp_mean, sd = d$sbp_sd)
         , dbp_percentile = stats::pnorm(dbp, mean = d$dbp_mean, sd = d$dbp_sd)
         , height_percentile = height_percentile)
  rtn
}

