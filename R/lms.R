#' Pediatric Growth Charts
#'
#' Based on the data provided by the CDC and the World Height Organization,
#' the distribution function, quantile function, and a z-score function for
#' several growth charts.
#'
#' @param q a vector of quantities
#' @param p a vector of probabilities
#' @param male integer value, 1 = male, 0 = female
#' @param age numeric age, in months
#' @param stature (height or length) in centimeters
#'
#' @return The \code{p_} method return values from the estimated distribution
#' function.  \code{q_} methods return values from the estimated quantile
#' function.  \code{z_} methods return standard scores, equivalent to
#' \code{\link[stats]{qnorm}}.
#'
#' @references
#' \url{https://www.cdc.gov/growthcharts/percentile_data_files.htm},
#' \url{https://www.who.int/tools/child-growth-standards/standards}
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
#' # Stature (aka: Lenght or Height) for Age
#' bmi_for_age
#' head_circumference_for_age
#' stature_for_age
#' weight_for_age
#' weight_for_stature
#'
#' #############################################################################
#' # Multiple patients, the age and male, length, height arguments can also be
#' # vectors
#' p_length_for_age_inf(q = 87, age = 28,  male = 0)
#' p_length_for_age_inf(q = 90, age = 30,  male = 1)
#' p_length_for_age_inf(q = c(87,90), age = c(28, 30),  male = c(0,1))
#'
#'
#' @name pediatric_vital_sign_distributions
NULL

#' @rdname pediatric_vital_sign_distributions
#' @export
pvsd <- function(x
                 , male
                 , age = NA_real_
                 , stature = NA_real_
                 , source = c("WHO", "CDC-2000")
                 , metric = c(  "bmi_for_age"
                              , "stature_for_age"
                              , "weight_for_age"
                              , "weight_for_stature"
                              , "head_circumference_for_age"
                   )
                 , type = c("distribution", "quantile", "zscore")
                 , ...) {

  type   <- match.arg(arg = type, several.ok = FALSE)
  metric <- match.arg(arg = metric, several.ok = FALSE)
  source <- match.arg(arg = source, several.ok = TRUE)

  lms <- v_get_lms(metric = metric, age = age, stature = stature, male = male, source = source, ...)

  rtn <-
    switch(type
           , distribution = Map(plms, x = x, l = lms[["L"]], m = lms[["M"]], s = lms[["S"]])
           , quantile     = Map(qlms, x = x, l = lms[["L"]], m = lms[["M"]], s = lms[["S"]])
           , zscore       = Map(zlms, x = x, l = lms[["L"]], m = lms[["M"]], s = lms[["S"]])
  )

  do.call(c, rtn)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
p_bmi_for_age <- function(q, male, age, source = getOption("pedbp_pvsd_source", "CDC-2000"), ...) {
  pvsd(x = q, male = male, age = age, source = source, metric = "bmi_for_age", type = "distribution", ...)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
q_bmi_for_age <- function(p, male, age, source = getOption("pedbp_pvsd_source", "CDC-2000"), ...) {
  pvsd(x = p, male = male, age = age, source = source, metric = "bmi_for_age", type = "quantile", ...)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
z_bmi_for_age <- function(q, male, age, source = getOption("pedbp_pvsd_source", "CDC-2000"), ...) {
  pvsd(x = q, male = male, age = age, source = source, metric = "bmi_for_age", type = "zscore", ...)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
p_head_circ_for_age <- function(q, male, age, source = getOption("pedbp_pvsd_source", "CDC-2000"), ...) {
  pvsd(x = q, male = male, age = age, source = source, metric = "head_circumference_for_age", type = "distribution", ...)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
q_head_circ_for_age <- function(p, male, age, source = getOption("pedbp_pvsd_source", "CDC-2000"), ...) {
  pvsd(x = p, male = male, age = age, source = source, metric = "head_circumference_for_age", type = "quantile", ...)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
z_head_circ_for_age <- function(q, male, age, source = getOption("pedbp_pvsd_source", "CDC-2000"), ...) {
  pvsd(x = q, male = male, age = age, source = source, metric = "head_circumference_for_age", type = "zscore", ...)
}


#' @rdname pediatric_vital_sign_distributions
#' @export
p_stature_for_age <- function(q, male, age, source = getOption("pedbp_pvsd_source", "CDC-2000"), ...) {
  pvsd(x = q, male = male, age = age, source = source, metric = "stature_for_age", type = "distribution", ...)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
q_stature_for_age <- function(p, male, age, source = getOption("pedbp_pvsd_source", "CDC-2000"), ...) {
  pvsd(x = p, male = male, age = age, source = source, metric = "stature_for_age", type = "quantile", ...)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
z_stature_for_age <- function(q, male, age, source = getOption("pedbp_pvsd_source", "CDC-2000"), ...) {
  pvsd(x = q, male = male, age = age, source = source, metric = "stature_for_age", type = "zscore", ...)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
p_weight_for_age <- function(q, male, age, source = getOption("pedbp_pvsd_source", "CDC-2000"), ...) {
  pvsd(x = q, male = male, age = age, source = source, metric = "weight_for_age", type = "distribution", ...)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
q_weight_for_age <- function(p, male, age, source = getOption("pedbp_pvsd_source", "CDC-2000"), ...) {
  pvsd(x = p, male = male, age = age, source = source, metric = "weight_for_age", type = "quantile", ...)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
z_weight_for_age <- function(q, male, age, source = getOption("pedbp_pvsd_source", "CDC-2000"), ...) {
  pvsd(x = q, male = male, age = age, source = source, metric = "weight_for_age", type = "zscore", ...)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
p_weight_for_stature <- function(q, male, stature, source = getOption("pedbp_pvsd_source", "CDC-2000"), ...) {
  pvsd(x = q, male = male, stature = stature, source = source, metric = "weight_for_stature", type = "distribution", ...)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
q_weight_for_stature <- function(p, male, stature, source = getOption("pedbp_pvsd_source", "CDC-2000"), ...) {
  pvsd(x = p, male = male, stature = stature, source = source, metric = "weight_for_stature", type = "quantile", ...)
}

#' @rdname pediatric_vital_sign_distributions
#' @export
z_weight_for_stature <- function(q, male, stature, source = getOption("pedbp_pvsd_source", "CDC-2000"), ...) {
  pvsd(x = q, male = male, stature = stature, source = source, metric = "weight_for_stature", type = "zscore", ...)
}


# non-exported functions

#' LMS Look Up Functions
#'
#' Functions to look up LMS values from the internal data set lms_data
#'
#' @param metric section of the look up table
#' @param male   0 = female; 1 = male
#' @param source "CDC-2000", or "WHO"
#' @param age    in months
#' @param stature height/length in centimeters
#'
#'
get_lms <-
  function(
    metric = c(  "bmi_for_age"
               , "stature_for_age"
               , "weight_for_age"
               , "weight_for_stature"
               , "head_circumference_for_age"
              )
  , male
  , source = c("WHO", "CDC-2000")
  , age = NA_real_
  , stature = NA_real_
  ) {

  metric <- match.arg(arg = metric, several.ok = FALSE)
  source <- match.arg(arg = source, several.ok = FALSE)
  stopifnot(length(male) == 1L, isTRUE(all.equal(male, 1)) | isTRUE(all.equal(male, 0)))

  idx <-
    (lms_data[["metric"]] == metric) &
    (lms_data[["male"]] == male) &
    (lms_data[["source"]] == source)

  if (grepl("_for_age", metric)) {
    stopifnot(!is.null(age), length(age) == 1L, age >= 0)

    min_age <- min(lms_data[["age"]][idx])
    max_age <- max(lms_data[["age"]][idx])

    if (age < min_age) {
      message(sprintf("age: %s, is below the min value in the look up table.  Using %s.", age, min_age))
      age <- min_age
    } else if (age > max_age) {
      message(sprintf("age: %s, is above the max value in the look up table.  Using %s.", age, max_age))
      age <- max_age
    }

    idx <- idx & (lms_data[["age"]] <= age)

  } else if (grepl("_for_stature", metric)) {
    stopifnot(!is.null(stature), length(stature) == 1L, stature >= 0)

    min_stature <- min(lms_data[["stature"]][idx])
    max_stature <- max(lms_data[["stature"]][idx])

    if (stature < min_stature) {
      message(sprintf("stature: %s, is below the min value in the look up table.  Using %s.", stature, min_stature))
      stature <- min_stature
    } else if (stature > max_stature) {
      message(sprintf("stature: %s, is above the max value in the look up table.  Using %s.", stature, max_stature))
      stature <- max_stature
    }

    idx <- idx & (lms_data[["stature"]] <= stature)

  } else {
    stop("what else could there be except for _for_age and _for_stature?")
  }

  rtn <- lms_data[max(which(idx)), ]
  rownames(rtn) <- NULL
  rtn
}

# v_get_lms is a vectorized version of get_lms.  get_lms uses a look up table
# and it was easier to think with single values there and vectorize at a higher
# level.
v_get_lms <-
  function(
    metric = c(  "bmi_for_age"
               , "stature_for_age"
               , "weight_for_age"
               , "weight_for_stature"
               , "head_circumference_for_age"
              )
    , male
    , source = c("WHO", "CDC-2000")
    , age = NA_real_
    , stature = NA_real_
    ) {

  metric <- match.arg(arg = metric, several.ok = FALSE)
  source <- match.arg(arg = source, several.ok = TRUE)

  rtn <-
    mapply(get_lms
           , metric = metric
           , male = male
           , source = source
           , age = age
           , stature = stature
           , SIMPLIFY = FALSE
    )
  rtn <- do.call(rbind, rtn)
  rownames(rtn) <- NULL
  rtn
}

zlms <- function(x, l, m, s, ...) {
  stopifnot(all(s >= 0))

  if (isTRUE(all.equal(0.0, l))) {
    z <- log( x / m) / s
  } else {
    z <- ( ((x / m) ^ l) - 1 ) / ( l * s)
  }
  z
}

plms <- function(x, l, m, s, ...) {
  z <- zlms(x, l, m, s)
  stats::pnorm(z, mean = 0, sd = 1)
}

qlms <- function(x, l, m, s, ...) {
  stopifnot(all(s >= 0))

  z <- stats::qnorm(x, mean = 0, sd = 1)

  if (isTRUE(all.equal(0.0, l))) {
    rtn <- m * exp(s * z)
  } else {
    rtn <- m * (1 + l * s * z) ^ (1 / l)
  }
  rtn
}
