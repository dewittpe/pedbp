#' Height Percentile
#'
#' Given the age, in years, sex, and height, in inches or centimeters, of a
#' child or adolescent, return the nearest height percentile as defined by the
#' data provided in Flynn et.al. (2017).
#'
#' @references
#'
#' Flynn JT, Kaelber DC, Baker-Smith CM, et al. Clinical Practice Guideline for
#' Screening and Management of High Blood Pressure in Children and Adolescents.
#' Pediatrics. 2017;140(3):e20171904
#'
#' @param age In years, expected integer value, will round to nearest integer
#' value if non-integer value is passed in.
#' @param male an integer value (1 = male, 0 = female)
#' @param sex an alternative to using the \code{male} argument, expecting a
#' character string of length one within the case insensitive set, \code{c("m",
#' "male", "f", "female")}.
#' @param height numeric value for the height of the subject
#' @param height_unit character string indicating if the \code{height} value is
#' in inches or centimeters.  Acceptable inputs are form the set \code{c("in",
#' "inches", "cm", "centimeters")}.
#' @param ... Pass through
#'
#' @examples
#'
#' height_percentile(12, sex = "M", 83.2, "cm")
#'
#' @export
height_percentile <- function(age, male, sex, height, height_unit, ...) {
  e <- new.env()
  utils::data(list = "bp_age_height", package = "pedbp", envir = e)

  stopifnot(length(age) == 1L)
  stopifnot(1 <= age & age <= 17)

  if (age %% 1 != 0) {
    age <- round(age, 0)
    warning(paste("rounding age to:", age))
  }

}

