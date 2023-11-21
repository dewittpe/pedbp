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

