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
