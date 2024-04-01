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
#' @inherit growth-standards
#'
#' @examples
#' # The 60th weight qualtile for a 1.2 meter tall male is
#' q_weight_for_height(p = 0.60, male = 1, height = 120, source = "CDC")
#' q_weight_for_height(p = 0.60, male = 1, height = 120, source = "WHO")
#'
#' # There are slight differences in the quantiles for length and height
#' q_weight_for_length(p = 0.60, male = 1, length = 97, source = "CDC")
#' q_weight_for_height(p = 0.60, male = 1, height = 97, source = "CDC")
#'
#' # percentiles and standard scores for a 14 kg, 88 cm tall/long male
#' p_weight_for_height(q = 14, male = 1, height = 88, source = "CDC")
#' p_weight_for_height(q = 14, male = 1, height = 88, source = "WHO")
#' p_weight_for_length(q = 14, male = 1, length = 88, source = "CDC")
#' p_weight_for_length(q = 14, male = 1, length = 88, source = "WHO")
#'
#' # correseponding standard scores
#' z_weight_for_height(q = 14, male = 1, height = 88, source = "CDC")
#' z_weight_for_height(q = 14, male = 1, height = 88, source = "WHO")
#' z_weight_for_length(q = 14, male = 1, length = 88, source = "CDC")
#' z_weight_for_length(q = 14, male = 1, length = 88, source = "WHO")
#'
#'
#' @aliases weight_for_length weight_for_height
#' @name weight_for_stature
NULL

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
