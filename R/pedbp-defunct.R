#' Defunct Functions
#'
#' These functions are fully defunct, no longer implemented, and will not work.
#'
#' @param q,p quantile or percentile
#' @param age age in months
#' @param male 0 = female; 1 = male
#' @param length centimeters
#'
#' @rdname pedbp-defunct
#' @export
p_length_for_age_inf <- function(q, age, male) {
  .Defunct(new = "p_length_for_age", package = "pedbp")
}

#' @rdname pedbp-defunct
#' @export
q_length_for_age_inf <- function(p, age, male) {
  .Defunct(new = "q_length_for_age", package = "pedbp")
}

#' @rdname pedbp-defunct
#' @export
z_length_for_age_inf <- function(q, age, male) {
  .Defunct(new = "z_length_for_age", package = "pedbp")
}

#' @rdname pedbp-defunct
#' @export
p_weight_for_age_inf <- function(q, age, male) {
  .Defunct(new = "p_weight_for_age", package = "pedbp")
}

#' @rdname pedbp-defunct
#' @export
q_weight_for_age_inf <- function(p, age, male) {
  .Defunct(new = "q_weight_for_age", package = "pedbp")
}

#' @rdname pedbp-defunct
#' @export
z_weight_for_age_inf <- function(q, age, male) {
  .Defunct(new = "z_weight_for_age", package = "pedbp")
}

#' @rdname pedbp-defunct
#' @export
p_weight_for_length_inf <- function(q, length, male) {
  .Defunct(new = "p_weight_for_length", package = "pedbp")
}

#' @rdname pedbp-defunct
#' @export
q_weight_for_length_inf <- function(p, length, male) {
  .Defunct(new = "q_weight_for_length", package = "pedbp")
}

#' @rdname pedbp-defunct
#' @export
z_weight_for_length_inf <- function(q, length, male) {
  .Defunct(new = "z_weight_for_length", package = "pedbp")
}
