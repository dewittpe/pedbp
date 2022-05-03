#' Pediatric Vital Sign Distributions
#'
#' Based on the data provided by the CDC, provide the distribution function,
#' quantile function, and a z-score function for one of eight vital signs by
#' another vital sign, e.g., weight for age.  Values are based on an LMS
#' approach.
#'
#' @param q a vector of quantites
#' @param p a vector of probabilities
#' @param age numeric age, in months
#' @param male integer value, 1 = male, 0 = female
#' @param height height, in cm, of the patient (age 2 - 20 years)
#' @param length length, in cm, of the patient (age under 3 years)
#'
#' @examples
#'
#' #############################################################################
#' # BMI for Age
#'
#' # A BMI of 18.2 for a 18.1 year old female is in the
#' p_bmi_for_age(q = 18.2, age = 18.1 * 12, male = 0)
#' # percentile.
#'
#' # The z-score is the same as qnorm(p)
#' qnorm(p_bmi_for_age(q = 18.2, age = 18.1 * 12, male = 0))
#' z_bmi_for_age(q = 18.2, age = 18.1 * 12, male = 0)
#'
#' # The 70th percentile of BMI for 15.4 year old males is
#' q_bmi_for_age(p = 0.70, age = 15.4 * 12, male = 1)
#'
#' #############################################################################
#' # Stature/Lenght/Height for Age
#'
#' # length_for_age_inf is for Infants are from 0 to 3 years (36 months)
#' # stature_for_age    is for pediatrics from 2 years (24 months) to 20 years
#' #                    (240 months)
#' # The overlap between these functions will produce slightly different values
#' # the kids between 24 and 36 months of age.
#' p_length_for_age_inf(87, age = 28, male = 0)
#' p_stature_for_age(87, age = 28, male = 0)
#' p_length_for_age_inf(q = 87, age = 28,  male = 0)
#'
#' #############################################################################
#' # Multiple patients, the age and male, length, height arguments can also be
#' # vectors
#' p_length_for_age_inf(q = 87, age = 28,  male = 0)
#' p_length_for_age_inf(q = 90, age = 30,  male = 1)
#' p_length_for_age_inf(q = c(87,90), age = c(28, 30),  male = c(0,1))
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
  lms <- v_get_lms(set = "length_for_age_inf", age = age, male = male)
  lms <- Map(append, lms, q)
  rtn <- lapply(lms, function(x,..) { plms(x[[4]], x$l, x$m, x$s) })
  rtn <- do.call(c, rtn)
  rtn
}

#' @rdname pediatric_vital_sign_distributions
#' @export
q_length_for_age_inf <- function(p, age, male) {
  #lms <- do.call(get_lms, list(set = "length_for_age_inf", age = age, male = male))
  #qlms(p, lms$l, lms$m, lms$s)
  lms <- v_get_lms(set = "length_for_age_inf", age = age, male = male)
  lms <- Map(append, lms, p)
  rtn <- lapply(lms, function(x,..) { qlms(x[[4]], x$l, x$m, x$s) })
  rtn <- do.call(c, rtn)
  rtn
}

#' @rdname pediatric_vital_sign_distributions
#' @export
z_length_for_age_inf <- function(q, age, male) {
  # lms <- do.call(get_lms, list(set = "length_for_age_inf", age = age, male = male))
  # zlms(q, lms$l, lms$m, lms$s)
  lms <- v_get_lms(set = "length_for_age_inf", age = age, male = male)
  lms <- Map(append, lms, q)
  rtn <- lapply(lms, function(x,..) { zlms(x[[4]], x$l, x$m, x$s) })
  rtn <- do.call(c, rtn)
  rtn
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
v_get_lms <- function(set = "", age, male, length = NULL, height = NULL) {
  stopifnot(length(age) == length(male))

  if (!is.null(length)) {
    stopifnot(length(age) == length(length))
  } else {
    length <- rep(NA_real_, length(age))
  }

  if (!is.null(height)) {
    stopifnot(length(age) == length(height))
  } else {
    height <- rep(NA_real_, length(age))
  }

  stopifnot(all(male %in% c(0, 1)))

  if (length(set) == 1L) {
    set <- rep(set, length(age))
  }

  rtn <- Map(get_lms, set = set, age = age, male = male, length = length, height = height)
  rtn <- unname(rtn)
  rtn
}

get_lms <- function(set = "", age = NA_real_, male, length = NA_real_, height = NA_real_) {

  stopifnot(length(male) == 1L)
  stopifnot(male == 0 | male == 1)

  if (set %in% c("length_for_age_inf", "weight_for_age_inf")) {
    stopifnot(length(age) == 1L)
    stopifnot(!is.na(age))
    d <- cdc_lms_data[cdc_lms_data$set == set, ]

    if (age < min(d$age) | age > max(d$age)) {
      stop(paste("age must be between", min(d$age), "and", max(d$age)))
    }

    d <- d[d$male == male, ]

    d1 <- d[d$age <= age, ]
    d2 <- d[d$age >  age, ]
    d  <- rbind(d1[nrow(d1), ], d2[1, ])

    l <- linear_interp(age, d[, "age"], d[, "l"])
    m <- linear_interp(age, d[, "age"], d[, "m"])
    s <- linear_interp(age, d[, "age"], d[, "s"])
  } else if (set %in% c("bmi_for_age", "head_circ_for_age", "stature_for_age", "weight_for_age")) {
    stopifnot(length(age) == 1L)
    stopifnot(!is.na(age))

    d <- cdc_lms_data[cdc_lms_data$set == set, ]

    if (age < min(d$age) | age > max(d$age)) {
      stop(paste("age must be between", min(d$age), "and", max(d$age)))
    }

    d <- d[d$male == male, ]

    d1 <- d[d$age <= age, ]
    d2 <- d[d$age >= age, ]
    d  <- rbind(d1[nrow(d1), ], d2[1, ])

    l <- linear_interp(age, d[, "age"], d[, "l"])
    m <- linear_interp(age, d[, "age"], d[, "m"])
    s <- linear_interp(age, d[, "age"], d[, "s"])
  } else if (set == "weight_for_length_inf") {
    stopifnot(length(length) == 1L)
    stopifnot(!is.na(length))
    d <- cdc_lms_data[cdc_lms_data$set == set, ]
    if (length < min(d$length) | max(d$length) < length) {
      stop(paste("length must be between", min(d$length), "and", max(d$length)))
    }
    d <- d[d$male == male, ]

    d1 <- d[d$length <= length, ]
    d2 <- d[d$length >= length, ]
    d  <- rbind(d1[nrow(d1), ], d2[1, ])

    l <- linear_interp(length, d[, "length"], d[, "l"])
    m <- linear_interp(length, d[, "length"], d[, "m"])
    s <- linear_interp(length, d[, "length"], d[, "s"])
  } else if (set == "weight_for_stature") {
    stopifnot(length(height) == 1L)
    stopifnot(!is.na(height))
    d <- cdc_lms_data[cdc_lms_data$set == set, ]
    if (height < min(d$height) | max(d$height) < height) {
      stop(paste("height must be between", min(d$height), "and", max(d$height)))
    }
    d <- d[d$male == male, ]

    d1 <- d[d$height <= height, ]
    d2 <- d[d$height >= height, ]
    d  <- rbind(d1[nrow(d1), ], d2[1, ])

    l <- linear_interp(height, d[, "height"], d[, "l"])
    m <- linear_interp(height, d[, "height"], d[, "m"])
    s <- linear_interp(height, d[, "height"], d[, "s"])
  } else {
    stop("unknown set")
  }

  list(l = l, m = m, s = s)
}

linear_interp <- function(x, xs, ys) {
  if (sum(is.na(xs) == 1)) {
    rtn <- ys[!is.na(xs)]
    return(rtn)
  }
  stopifnot(min(xs) <= x & x <= max(xs))
  if (isTRUE(all.equal(0.0, diff(xs)))) {
    rtn <- mean(ys)
  } else {
    rtn <- ys[1] + (diff(ys) / diff(xs)) * (x - xs[1])
  }
  rtn
}


zlms <- function(x, l, m, s) {
  stopifnot(all(s >= 0))

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
  stopifnot(all(s >= 0))

  z <- stats::qnorm(p, mean = 0, sd = 1)

  if (isTRUE(all.equal(0.0, l))) {
    rtn <- m * exp(s * z)
  } else {
    rtn <- m * (1 + l * s * z) ^ (1 / l)
  }
  rtn
}

