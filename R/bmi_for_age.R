#' BMI for Age - Pediatric Growth Standard
#'
#' BMI for age quantile, distribution, and zscore function based on LMS data
#' from the CDC and WHO.
#'
#' @inherit growth-standards
#'
#' @examples
#' # The 54th percentile BMI (kg * m^(-2)) for a six year (72 month) old female
#' # is
#' bmi <- q_bmi_for_age(p = 0.54, male = 0, age = 71.5)
#' bmi <- q_bmi_for_age(p = 0.54, male = 0, age = 71.6)
#' # lms_data[["bmi_for_age"]][["CDC"]][["Female"]]
#'
#' all.equal(p_bmi_for_age(q = bmi, male = 0, age = 72), 0.54)
#' all.equal(z_bmi_for_age(q = bmi, male = 0, age = 72), qnorm(0.54))
#'
#'
#' # Find the 29th percentile for females from ages 0 through 6 years in three
#' # month increments.  Sourcing the only CDC will generate a warning;
#' # suppressed in this example.
#' ages <- seq(0, 72, by = 3)
#' bmi_29 <-
#'   data.frame(
#'     age = ages
#'   , "CDC"     = suppressWarnings(q_bmi_for_age(p = 0.29, male = 0, age = ages, source = "CDC"))
#'   , "WHO"     = q_bmi_for_age(p = 0.29, male = 0, age = ages, source = "WHO")
#'   )
#'
#' bmi_29
#'
#' plot(
#'   x = bmi_29$age
#' , y = bmi_29$WHO
#' , col = 1
#' , pch = 1
#' , xlab = "Age (months)", ylab = "29th percentile BMI (kg * m^(-2))"
#' , type = "b"
#' )
#' points(x = bmi_29$age, y = bmi_29$CDC, col = 2, pch = 2, type = "b")
#' legend("bottomright", col = 1:2, pch = 1:2, legend = c("WHO", "CDC"))
#'
#' @name bmi_for_age
NULL

#' @rdname bmi_for_age
#' @export
p_bmi_for_age <- function(q, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = q, male = male, x = age, source = source, metric = "bmi_for_age", type = "distribution", ...)
}

#' @rdname bmi_for_age
#' @export
q_bmi_for_age <- function(p, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = p, male = male, x = age, source = source, metric = "bmi_for_age", type = "quantile", ...)
}

#' @rdname bmi_for_age
#' @export
z_bmi_for_age <- function(q, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = q, male = male, x = age, source = source, metric = "bmi_for_age", type = "zscore", ...)
}
