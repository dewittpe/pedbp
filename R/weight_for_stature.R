#' Weight for Stature - Pediatric Growth Standard
#'
#' Weight for stature quantile, distribution, and zscore function based on LMS data
#' from the CDC and WHO.
#'
#' Length or height values are used.  Length is assess when the patient is lying
#' down versus height when the patient is standing.  There is an implication of
#' younger patients being in the _for_length set.  There is some overlap in
#' numeric values of length and height.
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
