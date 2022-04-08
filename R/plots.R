#' Blood Pressure Chart
#'
#' Produce a graphic showing the regions of blood pressure percentiles by age
#' (in years) and height (percentile).
#'
#' @inheritParams height_percentile
#'
#' @export
bp_chart <- function(height, height_unit, age, male = NULL, sex = NULL, ...) {

  ht_percentile <- height_percentile(height, height_unit, age, male, ...)

  e <- new.env()
  utils::data(list = "bp_age_height", package = "pedbp", envir = e)

  idx <- e$bp_age_height$male == male & e$bp_age_height$age == age &
         e$bp_age_height$height_percentile == ht_percentile

  plot_data <- subset(e$bp_age_height, idx)



}
