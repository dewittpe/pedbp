#' Blood Pressure Charts
#'
#' Build blood pressure charts showing select percentile curves over age by sex,
#' height, and source.
#'
#' @inheritParams bp_distribution
#' @param bp character vector controlling if the systolic and/or the diastolic
#' pressures are plotted
#' @param p a numeric vector of the percentiles, provided in values between 0
#' and 1, to plot
#'
#' @examples
#'
#' bp_chart(male = 0:1)
#' bp_chart(male = 1)
#' bp_chart(male = 0)
#' bp_chart(male = 0, source = "gemelli1990")
#' bp_chart("sbp", male = 0, source = "gemelli1990")
#' bp_chart("dbp", male = 1, source = "gemelli1990")
#'
#' bp_chart("sbp", male = 1, source = "nhlbi")
#' bp_chart("sbp", male = 1, source = "flynn2017")
#'
#' # if you want to modify the plot, it might be helpful to see the data it is
#' # based on
#' g <- bp_chart(male = 1)
#' head(g$data)
#'
#' # here we color the background to show the source reference values
#' bkgrnd <- aggregate(x = age ~ male + bp + source, data = g$data, FUN = range)
#'
#' g +
#'   ggplot2::theme_bw() +
#'   ggplot2::geom_rect(
#'     data = bkgrnd,
#'     mapping = ggplot2::aes(xmin = age[, 1],
#'                            xmax = age[, 2] + 1,
#'                            ymin = -Inf,
#'                            ymax = Inf,
#'                            fill = source)
#'   ) +
#'   ggplot2::scale_fill_manual(
#'     name = "Data\nSource",
#'     values = c("gemelli1990" = ggplot2::alpha("#236192", 0.5)
#'                , "nhlbi"  = ggplot2::alpha("#6F263D", 0.5)
#'                , "lo2013"     = ggplot2::alpha("#A2AAAD", 0.5)
#'                ))
#'
#' @export
bp_chart <- function(bp = c("sbp", "dbp"), male = 0:1, height = NA, height_percentile = NA, default_height_percentile = 0.5, p = c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.90, 0.95, 0.99), source = getOption("pedbp_bp_source", "martin2022")) {
  stopifnot(length(male) %in% c(1L, 2L))
  stopifnot(male == 1 | male == 0)
  stopifnot(length(height) == 1)
  stopifnot(length(height_percentile) == 1)
  source <- match.arg(source, choices = c("martin2022", "gemelli1990", "nhlbi", "lo2013", "flynn2017"), several.ok = FALSE)
  bp <- match.arg(bp, several.ok = TRUE)

  d <- expand.grid(age = seq(1, 216, by = 1),
                   male = male,
                   height = height,
                   height_percentile = height_percentile,
                   p = p,
                   stringsAsFactors = FALSE)

  qs <- q_bp(p_sbp = d$p,
             p_dbp = d$p,
             age   = d$age,
             height = d$height,
             height_percentile = d$height_percentile,
             default_height_percentile = default_height_percentile,
             male = d$male,
             source = source)

  d <- cbind(d, qs[1:2], source = attr(qs, "bp_params")$source)
  d$plab <- factor(d$p, levels = p, labels = round(p * 100, digits = 1))

  sbp <- d[, -which(names(d) == "dbp")]
  dbp <- d[, -which(names(d) == "sbp")]
  names(sbp)[which(names(sbp) == "sbp")] <- "mmHg"
  names(dbp)[which(names(dbp) == "dbp")] <- "mmHg"
  d <- rbind(cbind(sbp, bp = "sbp"), cbind(dbp, bp = "dbp"))
  d <- d[d$bp %in% bp, ]
  d$bp <- factor(d$bp, levels = c("sbp", "dbp"), labels = c("Systolic", "Diastolic"))

  d <- d[!is.na(d$mmHg), ]

  ggplot2::ggplot(data = d) +
    ggplot2::geom_line(mapping = eval(substitute(ggplot2::aes(x = AGE, y = MMHG, group = P), list(AGE = as.name("age"), MMHG = as.name("mmHg"), P = as.name("p"))))) +
    ggplot2::geom_label(data = function(x) x[x$age == max(x$age), ],
                        mapping = eval(substitute(ggplot2::aes(x = age, y = mmHg, label = plab), list(AGE = as.name("age"), MMHG = as.name("mmHg"), PLAB = as.name("plab"))))) +
    ggplot2::facet_grid(bp ~ factor(male, 0:1, c("Female", "Male")), scales = 'free_y')  +
    ggplot2::scale_x_continuous(name = "Age (months)",
                                breaks = seq(0, max(d$age) + 12, by = ifelse(max(d$age) > 12, 12, 1))
                                ) +
    ggplot2::scale_y_continuous(name = "mmHg", breaks = seq(0, max(d$mmHg), by = 30))

}
