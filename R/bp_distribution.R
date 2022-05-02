#' Estimate Pediatric Blood Pressure Distribution
#'
#' @param q_sbp a vector of systolic blood pressures
#' @param q_dbp a vector of diastolic blood pressures
#' @param p_sbp a vector of systolic blood percentiles
#' @param p_dbp a vector of diastolic blood percentiles
#' @param age numeric age, in months
#' @param male integer value, 1 = male, 0 = female, indicating sex of the
#' patient
#' @param height numeric, in centimeters, can be missing.  This is the length
#' for patients under three years of age
#' @param height_percentile default height percentile to use if \code{height} is
#' missing.
#' @param ... not currently used
#'
#' @examples
#'
#' x <- p_bp( q_sbp = 100, q_dbp = 60, age = 8, male = 0)
#' x
#' str(x)
#'
#' x <- p_bp(q_sbp = c(NA, 82), q_dbp = c(60, 72), age = 9.2, male = 0)
#' x
#' str(x)
#'
#' x <- p_bp(q_sbp = c(NA, 82), q_dbp = c(60, 72), age = 29.2, male = 0, height = 82.8)
#' x
#' str(x)
#'
#' x <- q_bp(p_sbp = 0.78, p_dbp = 0.65, age = 8, male = 0)
#' x
#' str(x)
#'
#' @export
p_bp <- function(q_sbp, q_dbp, age, male, height = NA, height_percentile = 0.50, ...) {

  stopifnot(length(q_sbp) == length(q_dbp))

  d <- bp_params(age = age, male = male, height = height, height_percentile = height_percentile)

  rtn <-
    list(sbp_percentile = stats::pnorm(q_sbp, mean = d$sbp_mean, sd = d$sbp_sd)
      ,  dbp_percentile = stats::pnorm(q_dbp, mean = d$dbp_mean, sd = d$dbp_sd))
  attr(rtn, "bp_params") = d
  class(rtn) <- c("pedbp_bp", class(rtn))
  rtn
}

#' @export
q_bp <- function(p_sbp, p_dbp, age, male, height = NA, height_percentile = 0.50, ...) {

  stopifnot(length(p_sbp) == length(p_dbp))

  d <- bp_params(age = age, male = male, height = height, height_percentile = height_percentile)

  rtn <-
    list(sbp = stats::qnorm(p_sbp, mean = d$sbp_mean, sd = d$sbp_sd)
      ,  dbp = stats::qnorm(p_dbp, mean = d$dbp_mean, sd = d$dbp_sd))
  attr(rtn, "bp_params") = d
  class(rtn) <- c("pedbp_bp", class(rtn))
  rtn
}

bp_params <- function(age, male, height = NA, height_percentile = 0.50, ...) {
  stopifnot(length(age) == 1L)
  stopifnot(length(male) == 1L)
  stopifnot(all(age >=0) & all(age < 19 * 12))
  stopifnot(all(male %in% c(0L, 1L)))
  stopifnot(all(stats::na.omit(height > 0)))
  stopifnot(0 < height_percentile & height_percentile < 1)

  if (!is.na(height)) {
    # height_percentile <- get_height_percentile(age, male, height)
    if (age <= 36) {
      height_percentile <- p_length_for_age_inf(height, age = age, male = male)
    } else {
      height_percentile <- p_stature_for_age(height, age = age, male = male)
    }
  }

  e <- new.env()
  utils::data(list = "bp_parameters", package = "pedbp", envir = e)
  d <- e$bp_parameters[e$bp_parameters$male == male, ]

  if (age < 12) {
    d <- d[d$age < age, ]
    d <- d[d$age == max(d$age), ]
    height_percentile <- NA_real_
  } else if (is.na(height) & age >= 36) {
    d <- d[is.na(d$height_percentile), ]
    d <- d[d$age <= age, ]
    d <- d[d$age == max(d$age), ]
    height_percentile <- NA_real_
  } else {
    d <- d[d$age <= age, ]
    d <- d[d$age == max(d$age), ]
    d <- d[d$height_percentile == 5 | (d$height_percentile <= height_percentile * 100), ]
    d <- d[d$height_percentile == max(d$height_percentile), ]
  }

  stopifnot(nrow(d) == 1L)
  d
}



