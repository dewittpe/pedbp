#' Stature for Age - Pediatric Growth Standard
#'
#' Stature for age quantile, distribution, and zscore function based on LMS data
#' from the CDC and WHO.
#'
#' @inherit growth-standards
#'
#' @examples
#'
#' # Find the first quartile height for a 66 month old female.
#' # The quantile based on CDC data is slightly less than the quantile based on
#' # the data from the WHO
#' q_height_for_age(p = 0.25, age = 66, male = 0, source = c("CDC", "WHO"))
#'
#' # The 90th quantile length/height for a 24 month female: note that these
#' # values are similar, but not identical
#' q_length_for_age(p = 0.9, age = 24, male = 0, source = c("CDC"))
#' q_height_for_age(p = 0.9, age = 24, male = 0, source = c("CDC"))
#'
#' # Find the percentile for a 28 month old male with a stature (height/length)
#' # of 88 cm
#' p_height_for_age(q = 88, male = 1, age = 28, source = "CDC")
#' p_height_for_age(q = 88, male = 1, age = 28, source = "WHO")
#' p_length_for_age(q = 88, male = 1, age = 28, source = "CDC")
#' p_length_for_age(q = 88, male = 1, age = 28, source = "WHO")
#'
#' # correseponding standard scores
#' z_height_for_age(q = 88, male = 1, age = 28, source = "CDC")
#' z_height_for_age(q = 88, male = 1, age = 28, source = "WHO")
#' z_length_for_age(q = 88, male = 1, age = 28, source = "CDC")
#' z_length_for_age(q = 88, male = 1, age = 28, source = "WHO")
#'
#' @aliases height_for_age length_for_age
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
