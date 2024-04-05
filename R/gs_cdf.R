#' Cummulative Distribution Plots for Pediatric Growth Standards
#'
#' @inheritParams growth-standards
#' @param metric a character string denoting which growth metric to plot
#'
#' @return a \code{ggplot} object
#'
#' @seealso \itemize{
#' \item Distribuiton functions:
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
#' \item Other things:
#'   \itemize{
#'     \item \code{vignette(topic = "growth-standards", package = "pedbp")}
#'     \item package website \url{https://www.peteredewitt.com/pedbp}
#'   }
#' }
#'
#' @examples
#'
#' # Plot a # 13 year old male with a bmi of 21
#' gs_chart(metric = "bmi_for_age", male = 1) +
#'   ggplot2::geom_point(x = 13 * 12, y = 21)
#'
#' gs_cdf(metric = "bmi_for_age", male = 1, age = 13*12) +
#'   ggplot2::geom_point(x = 21, y = p_bmi_for_age(21, male = 1, age = 13*12))
#'
#' @export
gs_cdf <- function(metric, male, age, height, length, source = getOption("pedbp_pgs_source", "CDC")) {
  metric <- match.arg(metric, choices = c("bmi_for_age", "head_circumference_for_age", "height_for_age", "length_for_age", "weight_for_age", "weight_for_length", "weight_for_height"), several.ok = FALSE)
  source <- match.arg(source, choices = c("CDC", "WHO"), several.ok = FALSE)

  d <- data.frame(p = seq(0.00001, 0.999, length.out = 500))

  if (metric == "bmi_for_age") {
    d$q <- q_bmi_for_age(d$p, male = male, age = age, source = source)
    xlab <- "BMI"
  } else if (metric == "head_circumference_for_age") {
    d$q <- q_head_circumference_for_age(d$p, male = male, age = age, source = source)
    xlab <- "Head Circumference (cm)"
  } else if (metric == "height_for_age") {
    d$q <- q_height_for_age(d$p, male = male, age = age, source = source)
    xlab <- "Height (cm)"
  } else if (metric == "length_for_age") {
    d$q <- q_length_for_age(d$p, male = male, age = age, source = source)
    xlab <- "Length (cm)"
  } else if (metric == "weight_for_age") {
    d$q <- q_weight_for_age(d$p, male = male, age = age, source = source)
    xlab <- "Weight (kg)"
  } else if (metric == "weight_for_height") {
    d$q <- q_weight_for_height(d$p, male = male, height = height, source = source)
    xlab <- "Weight (kg)"
  } else if (metric == "weight_for_length") {
    d$q <- q_weight_for_length(d$p, male = male, length = length, source = source)
    xlab <- "Weight (kg)"
  }

  ggplot2::ggplot(data = d) +
    ggplot2::geom_line(mapping = eval(substitute(ggplot2::aes(x = Q, y = P), list(Q = as.name("q"), P = as.name("p"))))) +
    ggplot2::scale_y_continuous(name = "") +
    ggplot2::scale_x_continuous(name = xlab)
}

