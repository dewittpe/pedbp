library(pedbp)

################################################################################
# These tests encode the intended corrected behavior for the blood-pressure
# height-percentile scale bugs documented by the reprex scripts in the repo
# root. They are expected to fail until the implementation is patched.

x <- q_bp(
  p_sbp = 0.85,
  p_dbp = 0.95,
  age = 29.2,
  male = 0,
  height_percentile = 95,
  source = "flynn2017"
)

current_bp_cdf_replay <- p_bp(
  q_sbp = x$sbp,
  q_dbp = x$dbp,
  age = attr(x, "bp_params")$age,
  male = attr(x, "bp_params")$male,
  height_percentile = attr(x, "bp_params")$height_percentile,
  source = attr(x, "bp_params")$source
)

stopifnot(abs(current_bp_cdf_replay$sbp_p - 0.85) < 1e-7)
stopifnot(abs(current_bp_cdf_replay$dbp_p - 0.95) < 1e-7)


################################################################################
# bp_cdf.default should not force height_percentile = 0.5 into the BP engine.

bp_default <- p_bp(
  q_sbp = 103,
  q_dbp = 55,
  age = 96,
  male = 1
)

bp_cdf_default_replay <- p_bp(
  q_sbp = 103,
  q_dbp = 55,
  age = 96,
  male = 1,
  height_percentile = 50
)

stopifnot(identical(attr(bp_cdf_default_replay, "bp_params")$source, "nhlbi"))
stopifnot(identical(attr(bp_cdf_default_replay, "bp_params")$height_percentile, 50))
stopifnot(abs(bp_cdf_default_replay$sbp_p - 0.6524249) < 1e-7)
stopifnot(abs(bp_cdf_default_replay$dbp_p - 0.3605785) < 1e-7)
stopifnot(abs(bp_cdf_default_replay$sbp_p - bp_default$sbp_p) > 1e-7)
stopifnot(abs(bp_cdf_default_replay$dbp_p - bp_default$dbp_p) > 1e-7)


################################################################################
# Decimal height percentiles should not be silently reinterpreted as whole
# percentiles.

bp_95 <- p_bp(
  q_sbp = 100,
  q_dbp = 60,
  age = 44,
  male = 1,
  height_percentile = 95,
  source = "nhlbi"
)

bp_095 <- p_bp(
  q_sbp = 100,
  q_dbp = 60,
  age = 44,
  male = 1,
  height_percentile = 0.95,
  source = "nhlbi"
)

stopifnot(identical(attr(bp_095, "bp_params")$height_percentile, 5))
stopifnot(abs(bp_095$sbp_p - 0.9000536) < 1e-7)
stopifnot(abs(bp_095$dbp_p - 0.9152593) < 1e-7)
stopifnot(abs(bp_095$sbp_p - bp_95$sbp_p) > 1e-7)
stopifnot(abs(bp_095$dbp_p - bp_95$dbp_p) > 1e-7)

warn_test <- tryCatch(
  p_bp(
    q_sbp = 100,
    q_dbp = 60,
    age = 44,
    male = 1,
    height_percentile = 0.95,
    source = "nhlbi"
  ),
  warning = function(w) w
)

stopifnot(inherits(warn_test, "warning"))
stopifnot(grepl("0-100 scale", warn_test$message, fixed = TRUE))

################################################################################
##                                End of File                                 ##
################################################################################
