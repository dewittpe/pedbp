#' Blood Pressure Chart
#'
#' Produce a graphic showing the regions of blood pressure percentiles by age
#' (in years) and height (percentile).
#'
#' @param age age of the subject in years between 1 and 17.
#' @param height a numeric value
#' @param height_unit a character indicating if the \code{height} value is a
#' percentile (default), in inches, or centimeters.
#' @param ... Pass through
#'
#' @export
bp_chart <- function(age, height, height_unit = "percentile", ...) {

  stopifnot(!is.null(height_unit))
  height_unit <- tolower(height_unit)
  stopifnot(length(height_unit) == 1L & (height_unit %in% c("percentile", "in", "inches", "cm", "centimeters")))
}
