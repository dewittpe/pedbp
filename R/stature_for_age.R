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
