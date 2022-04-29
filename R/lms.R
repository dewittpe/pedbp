#' Pediatric Vital Sign Distributions
#'
#' Based on the data provided by the CDC, provide the distribution function,
#' quantile function, and a z-score function for one of eight vital signs by
#' another vital sign, e.g., weight for age.  Values are based on an LMS
#' approach.
#'
#' @param q a vector of quantites
#' @param p a vector of probabilities
#' @param age numeric age, in months, for one patient
#' @param male integer value, 1 = male, 0 = female, indicating sex of the
#' patient
#' @param height height, in cm, of the patient (age 2 - 20 years)
#' @param length length, in cm, of the patient (age under 3 years)
#'
#' @examples
#'
#' p_bmi_for_age(q = 16.455, age = 32, male = 1)
#' z_bmi_for_age(q = 16.455, age = 32, male = 1)
#' q_bmi_for_age(p = 0.58, age = 32, male = 1)
#'
#' p_head_circ_for_age(q = 16.455, age = 32, male = 1)
#' z_head_circ_for_age(q = 16.455, age = 32, male = 1)
#' q_head_circ_for_age(p = 0.58, age = 32, male = 1)
#'
#' p_length_for_age_inf(q = 16.455, age = 32, male = 1)
#' z_length_for_age_inf(q = 16.455, age = 32, male = 1)
#' q_length_for_age_inf(p = 0.58, age = 32, male = 1)
#'
#' p_stature_for_age(q = 16.455, age = 32, male = 1)
#' z_stature_for_age(q = 16.455, age = 32, male = 1)
#' q_stature_for_age(p = 0.58, age = 32, male = 1)
#'
#' p_weight_for_age(q = 4.455, age = 24.1, male = 1)
#' q_weight_for_age(p = 0.05, age = 24.1, male = 1)
#' z_weight_for_age(q = 4.455, age = 24.1, male = 1)
#'
#' p_weight_for_age_inf(q = 4.455, age = 2.41, male = 1)
#' q_weight_for_age_inf(p = 0.05, age = 2.41, male = 1)
#' z_weight_for_age_inf(q = 4.455, age = 2.41, male = 1)
#'
#' p_weight_for_length_inf(q = 4.455, length = 55, male = 1)
#' q_weight_for_length_inf(p = 0.05, length = 55, male = 1)
#' z_weight_for_length_inf(q = 4.455, length = 55, male = 1)
#'
#' p_weight_for_stature(q = 4.455, height = 95, male = 1)
#' q_weight_for_stature(p = 0.05, height = 95, male = 1)
#' z_weight_for_stature(q = 4.455, height = 95, male = 1)
#'
#' @references
#' \url{https://www.cdc.gov/growthcharts/percentile_data_files.htm}
#'
#' @name pediatric_vital_sign_distributions
NULL

#' @rdname pediatric_vital_sign_distributions
#' @export
p_bmi_for_age <- function(q, age, male) {
  lms <- do.call(get_lms, list(set = "bmi_for_age", age = age, male = male))
  plms(q, lms$l, lms$m, lms$s)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
q_bmi_for_age <- function(p, age, male) {
  lms <- do.call(get_lms, list(set = "bmi_for_age", age = age, male = male))
  qlms(p, lms$l, lms$m, lms$s)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
z_bmi_for_age <- function(q, age, male) {
  lms <- do.call(get_lms, list(set = "bmi_for_age", age = age, male = male))
  zlms(q, lms$l, lms$m, lms$s)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
p_head_circ_for_age <- function(q, age, male) {
  lms <- do.call(get_lms, list(set = "head_circ_for_age", age = age, male = male))
  plms(q, lms$l, lms$m, lms$s)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
q_head_circ_for_age <- function(p, age, male) {
  lms <- do.call(get_lms, list(set = "head_circ_for_age", age = age, male = male))
  qlms(p, lms$l, lms$m, lms$s)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
z_head_circ_for_age <- function(q, age, male) {
  lms <- do.call(get_lms, list(set = "head_circ_for_age", age = age, male = male))
  zlms(q, lms$l, lms$m, lms$s)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
p_length_for_age_inf <- function(q, age, male) {
  lms <- do.call(get_lms, list(set = "length_for_age_inf", age = age, male = male))
  plms(q, lms$l, lms$m, lms$s)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
q_length_for_age_inf <- function(p, age, male) {
  lms <- do.call(get_lms, list(set = "length_for_age_inf", age = age, male = male))
  qlms(p, lms$l, lms$m, lms$s)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
z_length_for_age_inf <- function(q, age, male) {
  lms <- do.call(get_lms, list(set = "length_for_age_inf", age = age, male = male))
  zlms(q, lms$l, lms$m, lms$s)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
p_stature_for_age <- function(q, age, male) {
  lms <- do.call(get_lms, list(set = "stature_for_age", age = age, male = male))
  plms(q, lms$l, lms$m, lms$s)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
q_stature_for_age <- function(p, age, male) {
  lms <- do.call(get_lms, list(set = "stature_for_age", age = age, male = male))
  qlms(p, lms$l, lms$m, lms$s)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
z_stature_for_age <- function(q, age, male) {
  lms <- do.call(get_lms, list(set = "stature_for_age", age = age, male = male))
  zlms(q, lms$l, lms$m, lms$s)
}


#' @rdname pediatric_vital_sign_distributions
#' @export
p_weight_for_age_inf <- function(q, age, male) {
  lms <- do.call(get_lms, list(set = "weight_for_age_inf", age = age, male = male))
  plms(q, lms$l, lms$m, lms$s)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
q_weight_for_age_inf <- function(p, age, male) {
  lms <- do.call(get_lms, list(set = "weight_for_age_inf", age = age, male = male))
  qlms(p, lms$l, lms$m, lms$s)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
z_weight_for_age_inf <- function(q, age, male) {
  lms <- do.call(get_lms, list(set = "weight_for_age_inf", age = age, male = male))
  zlms(q, lms$l, lms$m, lms$s)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
p_weight_for_age <- function(q, age, male) {
  lms <- do.call(get_lms, list(set = "weight_for_age", age = age, male = male))
  plms(q, lms$l, lms$m, lms$s)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
q_weight_for_age <- function(p, age, male) {
  lms <- do.call(get_lms, list(set = "weight_for_age", age = age, male = male))
  qlms(p, lms$l, lms$m, lms$s)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
z_weight_for_age <- function(q, age, male) {
  lms <- do.call(get_lms, list(set = "weight_for_age", age = age, male = male))
  zlms(q, lms$l, lms$m, lms$s)
}


#' @rdname pediatric_vital_sign_distributions
#' @export
p_weight_for_length_inf <- function(q, length, male) {
  lms <- do.call(get_lms, list(set = "weight_for_length_inf", length = length, male = male))
  plms(q, lms$l, lms$m, lms$s)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
q_weight_for_length_inf <- function(p, length, male) {
  lms <- do.call(get_lms, list(set = "weight_for_length_inf", length = length, male = male))
  qlms(p, lms$l, lms$m, lms$s)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
z_weight_for_length_inf <- function(q, length, male) {
  lms <- do.call(get_lms, list(set = "weight_for_length_inf", length = length, male = male))
  zlms(q, lms$l, lms$m, lms$s)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
p_weight_for_stature <- function(q, height, male) {
  lms <- do.call(get_lms, list(set = "weight_for_stature", height = height, male = male))
  plms(q, lms$l, lms$m, lms$s)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
q_weight_for_stature <- function(p, height, male) {
  lms <- do.call(get_lms, list(set = "weight_for_stature", height = height, male = male))
  qlms(p, lms$l, lms$m, lms$s)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
z_weight_for_stature <- function(q, height, male) {
  lms <- do.call(get_lms, list(set = "weight_for_stature", height = height, male = male))
  zlms(q, lms$l, lms$m, lms$s)
}


# non-exported functions
get_lms <- function(set = "", age = NA_real_, male, length = NA_real_, height = NA_real_) {

  stopifnot(length(male) == 1L)
  stopifnot(male == 0 | male == 1)

  if (set %in% c("length_for_age_inf", "weight_for_age_inf")) {
    stopifnot(length(age) == 1L)
    stopifnot(0 <= age & age <= 36)

    d <- cdc_lms_data[cdc_lms_data$set == set, ]
    d <- d[d$male == male, ]

    d1 <- d[d$age <= age, ]
    d2 <- d[d$age >  age, ]
    d  <- rbind(d1[nrow(d1), ], d2[1, ])

    l <- linear_interp(age, d[, "age"], d[, "l"])
    m <- linear_interp(age, d[, "age"], d[, "m"])
    s <- linear_interp(age, d[, "age"], d[, "s"])
  } else if (set %in% c("bmi_for_age", "head_circ_for_age", "stature_for_age", "weight_for_age")) {
    stopifnot(length(age) == 1L)
    stopifnot(24 <= age & age < 241)

    d <- cdc_lms_data[cdc_lms_data$set == set, ]
    d <- d[d$male == male, ]

    d1 <- d[d$age <= age, ]
    d2 <- d[d$age >  age, ]
    d  <- rbind(d1[nrow(d1), ], d2[1, ])

    l <- linear_interp(age, d[, "age"], d[, "l"])
    m <- linear_interp(age, d[, "age"], d[, "m"])
    s <- linear_interp(age, d[, "age"], d[, "s"])
  } else if (set == "weight_for_length_inf") {
    stopifnot(length(length) == 1L)
    d <- cdc_lms_data[cdc_lms_data$set == set, ]
    d <- d[d$male == male, ]

    d1 <- d[d$length <= length, ]
    d2 <- d[d$length >  length, ]
    d  <- rbind(d1[nrow(d1), ], d2[1, ])

    l <- linear_interp(age, d[, "length"], d[, "l"])
    m <- linear_interp(age, d[, "length"], d[, "m"])
    s <- linear_interp(age, d[, "length"], d[, "s"])
  } else if (set == "weight_for_stature") {
    stopifnot(length(height) == 1L)
    stopifnot(height > 0)
    d <- cdc_lms_data[cdc_lms_data$set == set, ]
    d <- d[d$male == male, ]

    d1 <- d[d$height <= height, ]
    d2 <- d[d$height >  height, ]
    d  <- rbind(d1[nrow(d1), ], d2[1, ])

    l <- linear_interp(age, d[, "height"], d[, "l"])
    m <- linear_interp(age, d[, "height"], d[, "m"])
    s <- linear_interp(age, d[, "height"], d[, "s"])
  } else {
    stop("unknown set")
  }

  list(l = l, m = m, s = s)
}

linear_interp <- function(x, xs, ys) {
  ys[1] + (diff(ys) / diff(xs)) * (x - xs[1])
}

#   - M: Median
#   - L: power of box-cox transform
#   - S: genralized ocefficient of variation

# For a given z-score, the value X is dertermined by:
# X = M * (1 + L * S * Z) ^ (1 / L), L != 0
#   = M exp(S * Z), L == 0
#
# Z score for a given value of X
#
# Z = (((X / M) ^ L ) - 1) / (L * S) ; L != 0
#   = log( X/M) / S                  ; L == 0 (natural log)


zlms <- function(x, l, m, s) {
  stopifnot(length(l) == 1L)
  stopifnot(length(m) == 1L)
  stopifnot(length(s) == 1L)
  stopifnot(s >= 0)

  if (isTRUE(all.equal(0.0, l))) {
    z <- log( x / m) / s
  } else {
    z <- ( ((x / m) ^ l) - 1 ) / ( l * s)
  }
  z
}

plms <- function(q, l, m, s) {
  z <- zlms(q, l, m, s)
  stats::pnorm(z, mean = 0, sd = 1)
}

qlms <- function(p, l, m, s) {
  stopifnot(length(l) == 1L)
  stopifnot(length(m) == 1L)
  stopifnot(length(s) == 1L)
  stopifnot(s >= 0)

  z <- stats::qnorm(p, mean = 0, sd = 1)

  if (isTRUE(all.equal(0.0, l))) {
    rtn <- m * exp(s * z)
  } else {
    rtn <- m * (1 + l * s * z) ^ (1 / l)
  }
  rtn
}


# d <- data.table::melt(stature_for_age, id.vars = c("Sex", "Agemos"), measure.vars = c("L", "M", "S"))
#
# ggplot2::ggplot(
#                 d[Agemos > 20 & Agemos < 38]
#                 ) +
#   ggplot2::aes(x = Agemos, y = value, color = factor(Sex)) +
#   ggplot2::geom_point() +
#   ggplot2::geom_line() +
#   ggplot2::facet_wrap( ~ variable, scales = "free_y")
