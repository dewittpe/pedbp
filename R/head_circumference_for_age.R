#' Head Circumference for Age - Pediatric Growth Standard
#'
#' BMI for age quantile, distribution, and zscore function based on LMS data
#' from the CDC and WHO.
#'
#' Note: CDC Recommends using WHO growth charts for infants and children ages 0
#' to 2 years of age in the U.S. and CDC growth charts to monitor growth for
#' children age 2 years and older in the U.S.
#'
#' @seealso
#'
#' @param q a vector of quantiles
#' @param p a vector of probabilities
#' @param male integer value, 1 = male, 0 = female
#' @param age numeric age, in months
#' @param source a character string denoting the data source providing the
#' parameters needed for the estimate.
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
#' @name head_circumference_for_age
NULL

#' @rdname head_circumference_for_age
#' @export
p_head_circ_for_age <- function(q, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = q, male = male, x = age, source = source, metric = "head_circumference_for_age", type = "distribution", ...)
}

#' @rdname head_circumference_for_age
#' @export
q_head_circ_for_age <- function(p, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = p, male = male, x = age, source = source, metric = "head_circumference_for_age", type = "quantile", ...)
}

#' @rdname head_circumference_for_age
#' @export
z_head_circ_for_age <- function(q, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = q, male = male, x = age, source = source, metric = "head_circumference_for_age", type = "zscore", ...)
}
