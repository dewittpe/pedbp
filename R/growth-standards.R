#' Pediatric Growth Standards
#'
#' Based on the data provided by the CDC and the World Height Organization,
#' the distribution function, quantile function, and a z-score function for
#' several growth charts.
#'
#' @details
#'
#' The \code{source} argument controls which data source is queried for the LMS
#' values.
#'
#' Note: CDC Recommends using WHO growth charts for infants and children ages 0
#' to 2 years of age in the U.S. and CDC growth charts to monitor growth for
#' children age 2 years and older in the U.S.
#'
#' @param q a vector of quantities
#' @param p a vector of probabilities
#' @param male integer value, 1 = male, 0 = female
#' @param age numeric age, in months
#' @param stature (height or length) in centimeters
#' @param source a character string denoting the data source providing the
#' parameters needed for the estimate.  See Details.
#' @param ... pass through
#'
#' @return The \code{p_} method return values from the estimated distribution
#' function.  \code{q_} methods return values from the estimated quantile
#' function.  \code{z_} methods return standard scores, equivalent to
#' \code{\link[stats]{qnorm}}.
#'
#' @references
#' \url{https://www.cdc.gov/growthcharts/percentile_data_files.htm},
#' \url{https://www.who.int/tools/child-growth-standards/standards}
#'
#' @examples
#'
#' #############################################################################
#' ## BMI for Age
#'
#' # The 54th percentile BMI (kg * m^(-2)) for a six year (72 month) old female
#' # is
#' bmi <- q_bmi_for_age(p = 0.54, male = 0, age = 72)
#'
#' all.equal(p_bmi_for_age(q = bmi, male = 0, age = 72), 0.54)
#' all.equal(z_bmi_for_age(q = bmi, male = 0, age = 72), qnorm(0.54))
#'
#'
#' # Find the 29th percentile for females from ages 0 through 4 years in three
#' # month increments.  Sourcing the only CDC will generate a warning;
#' # suppressed in this example.
#' ages <- seq(0, 48, by = 3)
#' bmi_29 <-
#'   data.frame(
#'     age = ages
#'   , "CDC-WHO" = q_bmi_for_age(p = 0.29, male = 0, age = ages, source = "CDC-WHO")
#'   , "CDC"     = suppressWarnings(q_bmi_for_age(p = 0.29, male = 0, age = ages, source = "CDC"))
#'   , "WHO"     = q_bmi_for_age(p = 0.29, male = 0, age = ages, source = "WHO")
#'   )
#'
#' bmi_29
#'
#' plot(
#'   x = bmi_29$age
#' , y = bmi_29$CDC.WHO, col = 1, pch = 1, cex = 2
#' , xlab = "Age (months)", ylab = "29th percentile BMI (kg * m^(-2))"
#' )
#' points(x = bmi_29$age, y = bmi_29$CDC, col = 2, pch = 2)
#' points(x = bmi_29$age, y = bmi_29$WHO, col = 3, pch = 3)
#' legend("bottomright", col = 1:3, pch = 1:3, legend = c("CDC-WHO", "CDC", "WHO"))
#'
#' #############################################################################
#' ## Distributions of other metrics are easy to get in the same way.
#' p <- runif(1)
#' age <- runif(1, min = 0, max = 18 * 12)
#' gender <- as.integer(runif(1) < 0.5)
#'
#' sfa <- q_stature_for_age(p = p, male = gender, age = age)
#' all.equal(p_stature_for_age(q = sfa, male = gender, age = age), p)
#' all.equal(z_stature_for_age(q = sfa, male = gender, age = age), qnorm(p))
#'
#' wfa <- q_weight_for_age(p = p, male = gender, age = age)
#' all.equal(p_weight_for_age(q = wfa, male = gender, age = age), p)
#' all.equal(z_weight_for_age(q = wfa, male = gender, age = age), qnorm(p))
#'
#' # weight for stature - range of values for stature:
#' # range(pedbp:::lms_data[pedbp:::lms_data$metric == "weight_for_stature", "stature"])
#' stature <- runif(1, min = 45, max = 121.5) # in centimeters
#' wfs <- q_weight_for_stature(p = p, male = gender, stature = stature)
#' all.equal(p_weight_for_stature(q = wfs, male = gender, stature = stature), p)
#' all.equal(z_weight_for_stature(q = wfs, male = gender, stature = stature), qnorm(p))
#'
#' # head circumference for age
#' # range(pedbp:::lms_data[pedbp:::lms_data$metric == "head_circumference_for_age", "age"])
#' age <- runif(1, 0, 36)
#' hcfa <- q_head_circ_for_age(p = p, male = gender, age = age)
#' all.equal(p_head_circ_for_age(q = hcfa, male = gender, age = age), p)
#' all.equal(z_head_circ_for_age(q = hcfa, male = gender, age = age), qnorm(p))
#'
#'
#' @name pediatric_growth_standards
NULL

#' @rdname pediatric_growth_standards
#' @export
p_bmi_for_age <- function(q, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = q, male = male, x = age, source = source, metric = "bmi_for_age", type = "distribution", ...)
}

#' @rdname pediatric_growth_standards
#' @export
q_bmi_for_age <- function(p, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(x = p, male = male, age = age, source = source, metric = "bmi_for_age", type = "quantile", ...)
}

#' @rdname pediatric_growth_standards
#' @export
z_bmi_for_age <- function(q, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(x = q, male = male, age = age, source = source, metric = "bmi_for_age", type = "zscore", ...)
}

#' @rdname pediatric_growth_standards
#' @export
p_head_circ_for_age <- function(q, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(x = q, male = male, age = age, source = source, metric = "head_circumference_for_age", type = "distribution", ...)
}

#' @rdname pediatric_growth_standards
#' @export
q_head_circ_for_age <- function(p, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(x = p, male = male, age = age, source = source, metric = "head_circumference_for_age", type = "quantile", ...)
}

#' @rdname pediatric_growth_standards
#' @export
z_head_circ_for_age <- function(q, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(x = q, male = male, age = age, source = source, metric = "head_circumference_for_age", type = "zscore", ...)
}


#' @rdname pediatric_growth_standards
#' @export
p_stature_for_age <- function(q, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(x = q, male = male, age = age, source = source, metric = "stature_for_age", type = "distribution", ...)
}

#' @rdname pediatric_growth_standards
#' @export
q_stature_for_age <- function(p, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(x = p, male = male, age = age, source = source, metric = "stature_for_age", type = "quantile", ...)
}

#' @rdname pediatric_growth_standards
#' @export
z_stature_for_age <- function(q, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(x = q, male = male, age = age, source = source, metric = "stature_for_age", type = "zscore", ...)
}

#' @rdname pediatric_growth_standards
#' @export
p_weight_for_age <- function(q, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(x = q, male = male, age = age, source = source, metric = "weight_for_age", type = "distribution", ...)
}

#' @rdname pediatric_growth_standards
#' @export
q_weight_for_age <- function(p, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(x = p, male = male, age = age, source = source, metric = "weight_for_age", type = "quantile", ...)
}

#' @rdname pediatric_growth_standards
#' @export
z_weight_for_age <- function(q, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(x = q, male = male, age = age, source = source, metric = "weight_for_age", type = "zscore", ...)
}

#' @rdname pediatric_growth_standards
#' @export
p_weight_for_stature <- function(q, male, stature, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(x = q, male = male, stature = stature, source = source, metric = "weight_for_stature", type = "distribution", ...)
}

#' @rdname pediatric_growth_standards
#' @export
q_weight_for_stature <- function(p, male, stature, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(x = p, male = male, stature = stature, source = source, metric = "weight_for_stature", type = "quantile", ...)
}

#' @rdname pediatric_growth_standards
#' @export
z_weight_for_stature <- function(q, male, stature, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(x = q, male = male, stature = stature, source = source, metric = "weight_for_stature", type = "zscore", ...)
}
