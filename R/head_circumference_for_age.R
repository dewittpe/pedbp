#' Head Circumference for Age - Pediatric Growth Standard
#'
#' Head Circumference for age quantile, distribution, and zscore function based
#' on LMS data from the CDC and WHO.
#'
#' @inherit growth-standards
#'
#' @examples
#'
#' # The median head circumfernce for a two-year-old female:
#' q_head_circumference_for_age(p = 0.5, male = 0, age = 24, source = "CDC")
#'
#' # Find the percentile for a 13 month old male with a head circumfernce of 46 cm:
#' p <- p_head_circumference_for_age(q = 46, male = 1, age = 13, source = "CDC")
#' p
#'
#' # the standard score is the quantile from a standard normal
#' z_head_circumference_for_age(q = 46, male = 1, age = 13, source = "CDC")
#' qnorm(p)
#'
#' \donttest{ # this will error -- WHO not yet implimented
#' q_head_circumference_for_age(0.5, male = 0, age = 24, source = "WHO")
#' }
#'
#' @name head_circumference_for_age
NULL

#' @rdname head_circumference_for_age
#' @export
p_head_circumference_for_age <- function(q, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = q, male = male, x = age, source = source, metric = "head_circumference_for_age", type = "distribution", ...)
}

#' @rdname head_circumference_for_age
#' @export
q_head_circumference_for_age <- function(p, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = p, male = male, x = age, source = source, metric = "head_circumference_for_age", type = "quantile", ...)
}

#' @rdname head_circumference_for_age
#' @export
z_head_circumference_for_age <- function(q, male, age, source = getOption("pedbp_pgs_source", "CDC"), ...) {
  cppPGSF(qp = q, male = male, x = age, source = source, metric = "head_circumference_for_age", type = "zscore", ...)
}
