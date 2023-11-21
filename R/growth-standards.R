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
#' @param q a vector of quantiles
#' @param p a vector of probabilities
#' @param male integer value, 1 = male, 0 = female
#' @param age numeric age, in months
#' @param height,length in centimeters
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
#' ## Distributions of other metrics are easy to get in the same way.
#' p <- runif(1)
#' age <- runif(1, min = 0, max = 18 * 12)
#' gender <- as.integer(runif(1) < 0.5)
#'
#' sfa <- q_height_for_age(p = p, male = gender, age = age)
#' all.equal(p_height_for_age(q = sfa, male = gender, age = age), p)
#' all.equal(z_height_for_age(q = sfa, male = gender, age = age), qnorm(p))
#'
#' wfa <- q_weight_for_age(p = p, male = gender, age = age)
#' all.equal(p_weight_for_age(q = wfa, male = gender, age = age), p)
#' all.equal(z_weight_for_age(q = wfa, male = gender, age = age), qnorm(p))
#'
#' # weight for height - range of values for height:
#' # range(pedbp:::lms_data[pedbp:::lms_data$metric == "weight_for_height", "height"])
#' height <- runif(1, min = 45, max = 121.5) # in centimeters
#' wfs <- q_weight_for_height(p = p, male = gender, height = height)
#' all.equal(p_weight_for_height(q = wfs, male = gender, height = height), p)
#' all.equal(z_weight_for_height(q = wfs, male = gender, height = height), qnorm(p))
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


#' @rdname stature_for_age
#' @export
p_height_for_age <- function(q, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = q, male = male, x = age, source = source, metric = "height_for_age", type = "distribution", ...)
}

#' @rdname stature_for_age
#' @export
q_height_for_age <- function(p, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = p, male = male, x = age, source = source, metric = "height_for_age", type = "quantile", ...)
}

#' @rdname stature_for_age
#' @export
z_height_for_age <- function(q, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = q, male = male, x = age, source = source, metric = "height_for_age", type = "zscore", ...)
}

#' @rdname stature_for_age
#' @export
p_length_for_age <- function(q, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = q, male = male, x = age, source = source, metric = "length_for_age", type = "distribution", ...)
}

#' @rdname stature_for_age
#' @export
q_length_for_age <- function(p, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = p, male = male, x = age, source = source, metric = "length_for_age", type = "quantile", ...)
}

#' @rdname stature_for_age
#' @export
z_length_for_age <- function(q, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = q, male = male, x = age, source = source, metric = "length_for_age", type = "zscore", ...)
}

#' @rdname weight_for_age
#' @export
p_weight_for_age <- function(q, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = q, male = male, x = age, source = source, metric = "weight_for_age", type = "distribution", ...)
}

#' @rdname weight_for_age
#' @export
q_weight_for_age <- function(p, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = p, male = male, x = age, source = source, metric = "weight_for_age", type = "quantile", ...)
}

#' @rdname weight_for_age
#' @export
z_weight_for_age <- function(q, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = q, male = male, x = age, source = source, metric = "weight_for_age", type = "zscore", ...)
}

#' @rdname weight_for_stature
#' @export
p_weight_for_height <- function(q, male, height, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = q, male = male, x = height, source = source, metric = "weight_for_height", type = "distribution", ...)
}

#' @rdname weight_for_stature
#' @export
q_weight_for_height <- function(p, male, height, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = p, male = male, x = height, source = source, metric = "weight_for_height", type = "quantile", ...)
}

#' @rdname weight_for_stature
#' @export
z_weight_for_height <- function(q, male, height, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = q, male = male, x = height, source = source, metric = "weight_for_height", type = "zscore", ...)
}

#' @rdname weight_for_stature
#' @export
p_weight_for_length <- function(q, male, length, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = q, male = male, x = length, source = source, metric = "weight_for_length", type = "distribution", ...)
}

#' @rdname weight_for_stature
#' @export
q_weight_for_length <- function(p, male, length, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = p, male = male, x = length, source = source, metric = "weight_for_length", type = "quantile", ...)
}

#' @rdname weight_for_stature
#' @export
z_weight_for_length <- function(q, male, length, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = q, male = male, x = length, source = source, metric = "weight_for_length", type = "zscore", ...)
}
