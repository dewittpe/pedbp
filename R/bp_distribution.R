#' Estimate Pediatric Blood Pressure Distribution
#'
#' Percentile and quantile functions for pediatric blood pressure.
#'
#' \code{source} is used to specify the method or source data sets by which the
#' percentiles are generated.  This can be controlled by the option
#' \code{"pedbp_bp_source"}.
#' End users are encouraged to set the option if not using the default so all
#' calls to these functions will use the same source.
#'
#' Options:
#' \itemize{
#'   \item \code{'martin2022'} (default) uses a combination of refernces to generate
#'   perentiles ages from 0 to 18, without or without known stature.  This was
#'   the only method impimented in version 1 of the pedbp package.
#'
#'   \item
#' }
#'
#'
#' @param q_sbp a vector of systolic blood pressures
#' @param q_dbp a vector of diastolic blood pressures
#' @param p_sbp a vector of systolic blood percentiles
#' @param p_dbp a vector of diastolic blood percentiles
#' @param age numeric age, in months
#' @param male integer value, 1 = male, 0 = female
#' @param height numeric, in centimeters, can be missing.
#' @param height_percentile default height percentile to use if \code{height} is
#' missing.
#' @param source the method, or data set, to use as the reference.  See Details.
#' @param ... not currently used
#'
#' @seealso \code{vignette("bp-distriution", package = "pedbp")}
#'
#' @return a \code{pedbp_bp} object.  This is a list of two numeric vectors:
#' \code{sbp_percentile} (systolic blood pressure) and \code{dbp_percentile}
#' (diastolic blood pressure).  Additionally, the \code{bp_params} attribute
#' provides details on the data source and parameters used in the percentile
#' estimates.
#'
#' @references
#'
#' @examples
#'
#' x <- p_bp(q_sbp = 100, q_dbp = 60, age = 8, male = 0)
#' x
#' str(x)
#'
#' x <- p_bp(q_sbp = c(NA, 82), q_dbp = c(60, 72), age = 9.2, male = 0)
#' x
#' str(x)
#'
#' x <- p_bp(q_sbp = c(NA, 82), q_dbp = c(60, 72), age = 29.2, male = 0, height = 82.8)
#' x
#' str(x)
#'
#' x <- q_bp(p_sbp = 0.78, p_dbp = 0.65, age = 8, male = 0)
#' x
#' str(x)
#'
#' #############################################################################
#' # compare results when height is known or unknown
#' p_bp(q_sbp = rep(100, 2),
#'      q_dbp = rep( 60, 2),
#'      age   = rep(35.75, 2),
#'      male  = c(0, 0),
#'      height = c(NA, 100))
#'
#' #############################################################################
#' # Working with multiple patients records
#' d <- read.csv(system.file("example_data", "for_batch.csv", package = "pedbp"))
#' d
#'
#' bp_percentiles <-
#'   p_bp(
#'       q_sbp = d$sbp..mmHg.
#'     , q_dbp = d$dbp..mmHg.
#'     , age   = d$age_months
#'     , male  = d$male
#'     )
#' bp_percentiles
#'
#' q_bp(
#'     p_sbp = bp_percentiles$sbp_percentile
#'   , p_dbp = bp_percentiles$dbp_percentile
#'   , age   = d$age_months
#'   , male  = d$male
#'   )
#'
#'
#' @name bp_distribution
NULL

#' @rdname bp_distribution
#' @export
p_bp <- function(q_sbp, q_dbp, age, male, height = NA, height_percentile = 0.50, source = getOption("pedbp_bp_source", "martin2022"), ...) {

  stopifnot(length(q_sbp) == length(q_dbp))

  if (length(age) == 1L) {
    age <- rep(age, length(q_dbp))
  } else if (length(age) > 1L) {
    stopifnot(length(q_sbp) == length(age), length(q_dbp) == length(age))
  } else {
    stop("length(age) needs to be at least 1L")
  }

  if (length(male) == 1L) {
    male <- rep(male, length(q_dbp))
  } else if (length(male) > 1L) {
    stopifnot(length(q_sbp) == length(male), length(q_dbp) == length(male))
  } else {
    stop("length(male) needs to be at least 1L")
  }

  if (length(height) == 1L) {
    height <- rep(height, length(q_dbp))
  } else if (length(height) > 1L) {
    stopifnot(length(q_sbp) == length(height), length(q_dbp) == length(height))
  } else {
    stop("length(height) needs to be at least 1L")
  }

  d <- v_bp_params(age = age, male = male, height = height, height_percentile = height_percentile)

  rtn <-
    list(sbp_percentile = stats::pnorm(q_sbp, mean = d$sbp_mean, sd = d$sbp_sd)
      ,  dbp_percentile = stats::pnorm(q_dbp, mean = d$dbp_mean, sd = d$dbp_sd))
  attr(rtn, "bp_params") <- d
  class(rtn) <- "pedbp_bp"
  rtn
}

#' @rdname bp_distribution
#' @export
q_bp <- function(p_sbp, p_dbp, age, male, height = NA, height_percentile = 0.50, source = getOption("pedbp_bp_source", "martin2022"), ...) {

  stopifnot(length(p_sbp) == length(p_dbp))

  if (length(age) == 1L) {
    age <- rep(age, length(p_dbp))
  } else if (length(age) > 1L) {
    stopifnot(length(p_sbp) == length(age), length(p_dbp) == length(age))
  } else {
    stop("length(age) needs to be at least 1L")
  }

  if (length(male) == 1L) {
    male <- rep(male, length(p_dbp))
  } else if (length(male) > 1L) {
    stopifnot(length(p_sbp) == length(male), length(p_dbp) == length(male))
  } else {
    stop("length(male) needs to be at least 1L")
  }

  if (length(height) == 1L) {
    height <- rep(height, length(p_dbp))
  } else if (length(height) > 1L) {
    stopifnot(length(p_sbp) == length(height), length(p_dbp) == length(height))
  } else {
    stop("length(height) needs to be at least 1L")
  }

  d <- v_bp_params(age = age, male = male, height = height, height_percentile = height_percentile, source = source)

  rtn <-
    list(sbp = stats::qnorm(p_sbp, mean = d$sbp_mean, sd = d$sbp_sd)
      ,  dbp = stats::qnorm(p_dbp, mean = d$dbp_mean, sd = d$dbp_sd))
  attr(rtn, "bp_params") <- d
  class(rtn) <- "pedbp_bp"
  rtn
}

#' @export
print.pedbp_bp <- function(x, ...) {
  print(x[1:2])
  invisible(x)
}


# v_bp_params is a vectorized version of bp_params.  The use of a look up table
# was easier to build assuming age, male, height were single values. v_bp_params
# will make functions easier to use for end users.

v_bp_params <- function(age, male, height, height_percentile = 0.50, source = getOption("pedbp_bp_source", "martin2022"), ...) {

  rtn <- Map(bp_params, age = age, male = male, height = height, height_percentile = height_percentile, source = source)
  do.call(rbind, rtn)

}


bp_params <- function(age, male, height = NA, height_percentile = 0.50, source = getOption("pedbp_bp_source", "martin2022"), ...) {
  stopifnot(length(age) == 1L)
  stopifnot(length(male) == 1L)
  stopifnot(all(age >=0) & all(age < 19 * 12))
  stopifnot(all(male %in% c(0L, 1L)))
  stopifnot(all(stats::na.omit(height > 0)))
  stopifnot(0 < height_percentile & height_percentile < 1)

  source <- match.arg(source, choices = c("martin2022", "gemelli1990", "nhlbi", "lo2013", "fourth"), several.ok = FALSE)

  # get the height_percentile if height is not NA
  # used for source %in% c("martin2022", "fourth")
  if (!is.na(height)) {
    if (age < 36) {
      height_percentile <- p_length_for_age(q = height, age = age, male = male, source = "WHO")
    } else {
      height_percentile <- p_height_for_age(q = height, age = age, male = male, source = "CDC")
    }
  }

  e <- new.env()
  utils::data(list = "bp_parameters", package = "pedbp", envir = e)
  d <- e$bp_parameters[e$bp_parameters$male == male, ]

  if (source == "martin2022") {
    if (age < 12) {
      d <- d[d$age == min(d$age) | d$age <= age, ]
      d <- d[d$age == max(d$age), ]
    } else if (is.na(height) & age >= 36) {
      d <- d[is.na(d$height_percentile), ]
      d <- d[((age >= 36) & (d$age <= age)), ]
      d <- d[d$age == max(d$age), ]
    } else {
      d <- d[!is.na(d$height_percentile), ]
      d <- d[d$age <= age, ]
      d <- d[d$age == max(d$age), ]
      d <- d[which.min(abs(d$height_percentile/100 - height_percentile)), ]
    }
  } else if (source %in% c("gemelli1990", "lo2013")) {
    d <- d[d$source == source, ]
    d <- d[d$age <= age, ]
    d <- d[d$age == max(d$age), ]
  } else if (source %in% c("nhlbi", "fourth")) {
    d <- d[d$source == source, ]
    d <- d[d$age <= age, ]
    d <- d[d$age == max(d$age), ]
    d <- d[which.min(abs(d$height_percentile/100 - height_percentile)), ]
  } else {
    stop("unknown source")
  }

  stopifnot(nrow(d) == 1L)
  d
}


