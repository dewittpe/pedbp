#' Approximate Height Percentiles
#'
#' Based on CDC data, height percentiles are estimated by age and sex, and by
#' interpolating CDC published quantiles with a Guassian distribution.
#'
#' The \code{\link{cdc_length_for_age}} data was interploated to derive a mean
#' and standard deviation for a Guassian approximation of the height
#' percentiles.
#'
#' @param age age in months
#' @param male integer value, 1 = male, 0 = female
#' @param height in centimeters
#' @param ... not currently used.
#'
#' @examples
#'
#'  e <- new.env()
#'  utils::data(list = "cdc_length_for_age", package = "pedbp", envir = e)
#'
#'  e$cdc_length_for_age[130, ]
#'  get_height_percentile(age = 78.5, male = 0, height = 109.0335)
#'  get_height_percentile(age = 78.5, male = 0, height = 122.4042)
#'
#' @export
get_height_percentile <- function(age, male, height, ...) {

  stopifnot(length(age) == 1L)
  stopifnot(length(male) == 1L)
  stopifnot(length(height) == 1L)
  stopifnot(!is.na(age))
  stopifnot(!is.na(male))
  stopifnot(!is.na(height))

  e <- new.env()
  utils::data(list = "ht_parameters", package = "pedbp", envir = e)

  dat <- e$ht_parameters[e$ht_parameters$male == male, ]
  dat <- dat[which.min(abs(age - dat$age)), ]
  stopifnot(nrow(dat) == 1L)

  stats::pnorm(height, mean = dat$height_mean, sd = dat$height_sd)
}
