#' Weight for Age - Pediatric Growth Standard
#'
#' Weight for age quantile, distribution, and zscore function based on LMS data
#' from the CDC and WHO.
#'
#' @inherit growth-standards
#'
#' @examples
#'
#' # find the 80th quantile for 56 month old females;
#' # note - slight difference between CDC and WHO.
#' q_weight_for_age(p = 0.80, age = 56, male = 0, source = c("CDC", "WHO"))
#'
#' # the percentiles for 42 kg 13 year old males:
#' p_weight_for_age(q = 42, age = 13 * 12, male = 0, source = "CDC")
#' p_weight_for_age(q = 42, age = 13 * 12, male = 0, source = "WHO")
#'
#' z_weight_for_age(q = 42, age = 13 * 12, male = 0, source = "CDC")
#' z_weight_for_age(q = 42, age = 13 * 12, male = 0, source = "WHO")
#'
#' @name weight_for_age
NULL

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

