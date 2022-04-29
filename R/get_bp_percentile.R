#' Estimate Pediatric Blood Pressure Percentiles
#'
#' Estimate the percentile for blood pressure and height for a pediatric patient
#' based on sex and age and height.  Sex and age are required, height is not.
#'
#' @param sbp Systolic blood pressure, mmHg
#' @param dbp Systolic blood pressure, mmHg
#' @param age age in months
#' @param male integer value, 1 = male, 0 = female
#' @param height numeric, in centimeters, can be missing.
#' @param height_percentile default height percentile to use if \code{height} is
#' missing.
#' @param ... not currently used
#'
#' @examples
#' get_bp_percentile(sbp = 100, dbp = 60, age = 8, male = 0)
#' @export
get_bp_percentile <- function(sbp, dbp, age, male, height = NA, height_percentile = 0.50, ...) {
  stopifnot(length(age) == 1L)
  stopifnot(length(male) == 1L)
  stopifnot(all(age >=0) & all(age < 19))
  stopifnot(all(male %in% c(0L, 1L)))
  stopifnot(all(stats::na.omit(height > 0)))

  if (!is.na(height)) {
    height_percentile <- get_height_percentile(age, male, height)
  }

  rtn <- list(sbp_percentile = NA, dbp_percentile = NA, height_percentile = height_percentile)

  # think through this logic more -- It could be better
  if (age < 12) {
    rtn <- get_bp_percentile_gemelli(sbp, dbp, age, male)
  } else if (!is.na(height)) {
    rtn <- get_bp_percentile_flynn2017(sbp, dbp, age, male, height_percentile)
  } else if (age < 3) {
    rtn <- get_bp_percentile_flynn2017(sbp, dbp, age, male, height_percentile)
  } else {
    rtn <- get_bp_percentile_lo2013(sbp, dbp, age, male)
  }


  rtn
}

get_bp_percentile_gemelli <- function(sbp, dbp, age, male) {
  e <- new.env()
  utils::data(list = "gemelli1990", package = "pedbp", envir = e)
  idx <- which(e$gemelli1990$male == male)
  dat <- e$gemelli1990[idx, ]
  dat <- dat[which.min(abs(age - dat$age)), ]
  list(
       sbp_percentile = stats::pnorm(sbp, mean = dat$mean_sbp, sd = dat$sd_sbp),
       dbp_percentile = stats::pnorm(dbp, mean = dat$mean_dbp, sd = dat$sd_dbp),
       height_percentile = NA_real_
       )
}

get_bp_percentile_flynn2017 <- function(sbp, dbp, age, male, height_percentile) {
}

get_bp_percentile_lo2013 <- function(sbp, dbp, age, male, height_percentile) {
}
