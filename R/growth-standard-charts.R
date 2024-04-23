#' Pediatric Growth Standard Charts
#'
#' Growth standards based on data from the Centers for Disease Control and the
#' World Health Organization.
#'
#' @inherit growth-standards
#' @param metric character string, one of the growth standards
#' @param p a numeric vector of the probabilities, provided in values between 0
#' and 1, to plot
#'
#' @return
#'
#' A \code{ggplot} object
#'
#' @references
#' \url{https://www.cdc.gov/growthcharts/percentile_data_files.htm},
#' \url{https://www.who.int/tools/child-growth-standards/standards}
#'
#' @seealso \itemize{
#' \item Distribution functions:
#'   \itemize{
#'     \item \code{\link{bmi_for_age}}
#'     \item \code{\link{head_circumference_for_age}}
#'     \item \code{\link{height_for_age}}
#'     \item \code{\link{length_for_age}}
#'     \item \code{\link{weight_for_age}}
#'     \item \code{\link{weight_for_length}}
#'     \item \code{\link{weight_for_height}}
#'   }
#' \item Plotting functions:
#'   \itemize{
#'     \item \code{\link{gs_chart}}
#'     \item \code{\link{gs_cdf}}
#'   }
#' \item Vignette:
#'   \itemize{
#'     \item \code{vignette(topic = "growth-standards", package = "pedbp")}
#'   }
#' }
#'
#' @examples
#'
#' gs_chart("bmi_for_age", male = 0)
#' gs_chart("bmi_for_age", male = 1)
#' gs_chart("bmi_for_age", male = 0:1)
#'
#' # add a point for a specific patient
#' pt <- data.frame(p = 0.82, age = 156, bmi = q_bmi_for_age(p = 0.82, male = 1, age = 156))
#' gs_chart("bmi_for_age", male = 1) +
#'   ggplot2::geom_point(data = pt, mapping = ggplot2::aes(x = age, y = bmi))
#'
#' # select specific percentiles to plot
#' gs_chart("weight_for_height", male = 0:1, p = c(0.10, 0.80))
#'
#' @export
gs_chart <- function(metric, male = 0:1, source = getOption("pedbp_pgs_source", "CDC"), p = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99)) {
  metric <- match.arg(metric, choices = c("bmi_for_age", "head_circumference_for_age", "height_for_age", "length_for_age", "weight_for_age", "weight_for_length", "weight_for_height"), several.ok = FALSE)
  source <- match.arg(source, choices = c("CDC", "WHO"), several.ok = FALSE)
  stopifnot(male == 0 | male == 1)
  stopifnot(all(p > 0 & p < 1.0))

  d <- do.call(rbind, lms_data[[metric]][[source]])
  d <- d[d$male %in% male, ]

  xnm <- switch(metric,
                "bmi_for_age" = "age",
                "head_circumference_for_age" = "age",
                "height_for_age" = "age",
                "length_for_age" = "age",
                "weight_for_age" = "age",
                "weight_for_height" = "height",
                "weight_for_length" = "length")

  d <- d[, c("metric", "male", xnm, "L", "M", "S")]

  qs <- apply(d[, c("L", "M", "S")], 1, function(x) {qlms(p, x["L"], x["M"], x["S"])})
  qs <- t(qs)
  qs <- data.frame(qs)
  qs <- Map(function(x, p) cbind(d[, c("male", xnm)], value = x, p = p), x = qs, p = p)
  qs <- do.call(rbind, qs)
  qs$p <- factor(qs$p, levels = sort(p), labels = paste(sort(p) * 100, "%ile"))

  yaxis <- switch(metric,
                  "bmi_for_age" = ggplot2::scale_y_continuous(name = "BMI"),
                  "head_circumference_for_age" = ggplot2::scale_y_continuous(name = "Head Circumference (cm)"),
                  "height_for_age" = ggplot2::scale_y_continuous(name = "Height (cm)"),
                  "length_for_age" = ggplot2::scale_y_continuous(name = "Length (cm)"),
                  "weight_for_age" = ggplot2::scale_y_continuous(name = "Weight (kg)"),
                  "weight_for_height" = ggplot2::scale_y_continuous(name = "Weight (kg)"),
                  "weight_for_length" = ggplot2::scale_y_continuous(name = "Weight (kg)"))

  xaxis <- switch(metric,
                  "weight_for_height" = ggplot2::scale_x_continuous(name = "Height (cm)"),
                  "weight_for_length" = ggplot2::scale_x_continuous(name = "Length (cm)"),
                  ggplot2::scale_x_continuous(name = "Age"
                                              , breaks = seq(0, max(d$age) + 12, by = 12)
                                              , labels = paste(
                                                               paste0(seq(0, max(d$age) + 12, by = 12), "m")
                                                               , paste0(seq(0, max(d$age) + 12, by = 12) / 12, "yr")
                                                               , sep = "\n")
                                              ))

  ggplot2::ggplot(data = qs) +
    ggplot2::geom_line(data = qs, mapping = eval(substitute(ggplot2::aes(x = X, y = value, color = p), list(X = as.name(xnm))))) +
    ggplot2::facet_wrap( ~ factor(male, 0:1, c("Female", "Male"))) +
    xaxis +
    yaxis +
    ggplot2::theme(legend.title = ggplot2::element_blank(), legend.position = "bottom")
}
