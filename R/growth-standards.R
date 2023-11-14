#' Pediatric Growth Standards
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
#' @name pediatric_growth_standards
NULL

# pgsf is a non-exported work horse function that is called with specific values
# from the p_*, q_*, z_* functions which are exported
pgsf <- function(x
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
  source <- match.arg(arg = source, several.ok = FALSE)

  if (is.null(age)) {
    age <- NA_real_
  }
  if (is.null(stature)) {
    stature <- NA_real_
  }

  lms <- v_get_lms(metric = metric, age = age, stature = stature, male = male, source = source, ...)

  rtn <-
    switch(type
           , distribution = Map(plms, x = x, l = lms[["L"]], m = lms[["M"]], s = lms[["S"]])
           , quantile     = Map(qlms, x = x, l = lms[["L"]], m = lms[["M"]], s = lms[["S"]])
           , zscore       = Map(zlms, x = x, l = lms[["L"]], m = lms[["M"]], s = lms[["S"]])
  )

  do.call(c, rtn)
}

#' @rdname pediatric_growth_standards
#' @export
p_bmi_for_age <- function(q, male, age, source = getOption("pedbp_pgsf_source", "CDC-2000"), ...) {
  pgsf(x = q, male = male, age = age, source = source, metric = "bmi_for_age", type = "distribution", ...)
}

#' @rdname pediatric_growth_standards
#' @export
q_bmi_for_age <- function(p, male, age, source = getOption("pedbp_pgsf_source", "CDC-2000"), ...) {
  pgsf(x = p, male = male, age = age, source = source, metric = "bmi_for_age", type = "quantile", ...)
}

#' @rdname pediatric_growth_standards
#' @export
z_bmi_for_age <- function(q, male, age, source = getOption("pedbp_pgsf_source", "CDC-2000"), ...) {
  pgsf(x = q, male = male, age = age, source = source, metric = "bmi_for_age", type = "zscore", ...)
}

#' @rdname pediatric_growth_standards
#' @export
p_head_circ_for_age <- function(q, male, age, source = getOption("pedbp_pgsf_source", "CDC-2000"), ...) {
  pgsf(x = q, male = male, age = age, source = source, metric = "head_circumference_for_age", type = "distribution", ...)
}

#' @rdname pediatric_growth_standards
#' @export
q_head_circ_for_age <- function(p, male, age, source = getOption("pedbp_pgsf_source", "CDC-2000"), ...) {
  pgsf(x = p, male = male, age = age, source = source, metric = "head_circumference_for_age", type = "quantile", ...)
}

#' @rdname pediatric_growth_standards
#' @export
z_head_circ_for_age <- function(q, male, age, source = getOption("pedbp_pgsf_source", "CDC-2000"), ...) {
  pgsf(x = q, male = male, age = age, source = source, metric = "head_circumference_for_age", type = "zscore", ...)
}


#' @rdname pediatric_growth_standards
#' @export
p_stature_for_age <- function(q, male, age, source = getOption("pedbp_pgsf_source", "CDC-2000"), ...) {
  pgsf(x = q, male = male, age = age, source = source, metric = "stature_for_age", type = "distribution", ...)
}

#' @rdname pediatric_growth_standards
#' @export
q_stature_for_age <- function(p, male, age, source = getOption("pedbp_pgsf_source", "CDC-2000"), ...) {
  pgsf(x = p, male = male, age = age, source = source, metric = "stature_for_age", type = "quantile", ...)
}

#' @rdname pediatric_growth_standards
#' @export
z_stature_for_age <- function(q, male, age, source = getOption("pedbp_pgsf_source", "CDC-2000"), ...) {
  pgsf(x = q, male = male, age = age, source = source, metric = "stature_for_age", type = "zscore", ...)
}

#' @rdname pediatric_growth_standards
#' @export
p_weight_for_age <- function(q, male, age, source = getOption("pedbp_pgsf_source", "CDC-2000"), ...) {
  pgsf(x = q, male = male, age = age, source = source, metric = "weight_for_age", type = "distribution", ...)
}

#' @rdname pediatric_growth_standards
#' @export
q_weight_for_age <- function(p, male, age, source = getOption("pedbp_pgsf_source", "CDC-2000"), ...) {
  pgsf(x = p, male = male, age = age, source = source, metric = "weight_for_age", type = "quantile", ...)
}

#' @rdname pediatric_growth_standards
#' @export
z_weight_for_age <- function(q, male, age, source = getOption("pedbp_pgsf_source", "CDC-2000"), ...) {
  pgsf(x = q, male = male, age = age, source = source, metric = "weight_for_age", type = "zscore", ...)
}

#' @rdname pediatric_growth_standards
#' @export
p_weight_for_stature <- function(q, male, stature, source = getOption("pedbp_pgsf_source", "CDC-2000"), ...) {
  pgsf(x = q, male = male, stature = stature, source = source, metric = "weight_for_stature", type = "distribution", ...)
}

#' @rdname pediatric_growth_standards
#' @export
q_weight_for_stature <- function(p, male, stature, source = getOption("pedbp_pgsf_source", "CDC-2000"), ...) {
  pgsf(x = p, male = male, stature = stature, source = source, metric = "weight_for_stature", type = "quantile", ...)
}

#' @rdname pediatric_growth_standards
#' @export
z_weight_for_stature <- function(q, male, stature, source = getOption("pedbp_pgsf_source", "CDC-2000"), ...) {
  pgsf(x = q, male = male, stature = stature, source = source, metric = "weight_for_stature", type = "zscore", ...)
}
