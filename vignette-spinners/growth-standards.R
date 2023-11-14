#'---
#'title: "Growth Standards"
#'output:
#'  rmarkdown::html_vignette:
#'    toc: true
#'    number_sections: true
#'bibliography: references.bib
#'vignette: >
#'  %\VignetteIndexEntry{Growth Standards}
#'  %\VignetteEngine{knitr::rmarkdown}
#'  %\VignetteEncoding{UTF-8}
#'---
#'
#+ label = "setup", include = FALSE
#/*
devtools::load_all() # load the dev version while editting
#*/
knitr::opts_chunk$set(collapse = TRUE, fig.align = "center")
library(pedbp)
#'
#' # Introduction
#'
#'
#' # CDC Growth Charts
#'
#+ label = 'lms_data_table', include = FALSE
lms <- data.table::setDT(data.table::copy(pedbp:::lms_data))
cdc_lms <- subset(lms, source == "CDC")
#'
#' Using the [Percentile Data Files with LMS values](https://www.cdc.gov/growthcharts/percentile_data_files.htm)
#' provided by the CDC, we provide eight distribution tables which have been
#' combinded into five:
#'
#' | CDC Source                    | pedbp metric               |
#' | :---------------------------- | :------------------------- |
#' | BMI for age                   | bmi_for_age                |
#' | head circumference for age    | head_circumference_for_age |
#' | length for age for infants    | stature_for_age            |
#' | stature for age               | stature_for_age            |
#' | weight for age for infants    | weight_for_age             |
#' | weight for age                | weight_for_age             |
#' | weight for length for infants | weight_for_stature         |
#' | weight for stature            | weight_for_stature         |
#'
#' All lengths/heights are in centimeters, ages in months, and weights in
#' kilograms.
#'
#' The length-for-age and stature-for-age methods were needed for the blood
#' pressure methods above.
#'
#' All methods use the published LMS parameters to define z-scores,
#' percentiles, and quantiles for skewed distributions.  L is a $\lambda$
#' parameter, the Box-Cox transformation power; $M$ the median value, and $S$ a
#' generalized coefficient of variation.  For a given percentile or z-score, the
#' corresponding physical measurement, $X,$ is defined as
#'
#' $$X = \begin{cases}
#'    M \left(1 + \lambda S Z \right)^{\frac{1}{\lambda}} & \lambda \neq 0 \\
#'    M \exp\left( S Z \right) & \lambda = 0.
#' \end{cases}$$
#'
#' From this we can get the z-score for a given measurement $X:$
#'
#' $$ Z = \begin{cases}
#'   \frac{\left(\frac{X}{M}\right)^{\lambda} - 1}{\lambda S} & \lambda \neq 0 \\
#'   \frac{\log\left(\frac{X}{M}\right) }{ S } & \lambda = 0.
#'   \end{cases}$$
#'
#' Percentiles are determined using the standard normal distribution of z-scores.
#'
#' For all eight of the noted methods we provide a distribution function,
#' quantile function, and function that returns z-scores.
#'
#' Estimates for finer differences in age, for example, are possible for these
#' methods than the blood pressure methods.  This is due to the permissible
#' linear interpolation of the LMS parameters for the CDC charts whereas the
#' blood pressure assessment is restricted to values within a look up table.
#'
#' ## Length and Stature For Age
#'
#' A 13 year old male standing 154 cm tall is in the
{{ paste0(round(p_stature_for_age(q = 154, age = 13 * 12, male = 1L, source = "CDC") * 100, 2), "th") }}
#' percentile:
p_stature_for_age(q = 154, age = 13 * 12, male = 1L)

#'
#' To find the height corresponding to the 50th, 60th, and 75th percentiles for
#' height for 9.5-year old girls:
q_stature_for_age(p = c(0.50, 0.60, 0.75), age = 9.5 * 12, male = 0L)

#'
#' If you want the standard score for a percentile, you can use qnorm around
#' p_stature_for_age, or simply call z_stature_for_age.
qnorm(p_stature_for_age(q = 154, age = 13 * 12, male = 1L))
z_stature_for_age(q = 154, age = 13 * 12, male = 1L)

#'
#' A length/height for age chart based on the CDC data:
#+ echo = FALSE
lfa <- data.table::CJ(p = c(0.01, 0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95, 0.99),
                      age = seq(0, 18*12, by = 3),
                      male = 0:1)

lfa[age <  36, l := q_stature_for_age(p = p, age = age, male = male), by = .(p, age, male)]
lfa[age >= 36, l := q_stature_for_age(p = p, age = age, male = male), by = .(p, age, male)]
lfa[, lab := paste(p * 100, "%")]
lfa[, male := factor(male, 0:1, c("Female", "Male"))]
percentile_factor <- function(p) {
  factor(p, levels = sort(unique(p)), labels = paste0(sort(unique(p)) * 100, "th"))
}
lfa[, p := percentile_factor(p)]

g <- function(lfa) {
  ggplot2::ggplot(lfa) +
    ggplot2::theme_bw() +
    ggplot2::aes(x = age, y = l, color = p) +
    ggplot2::geom_line() +
    ggplot2::facet_wrap( ~ male) +
    ggplot2::scale_x_continuous(name = "Age"
                                , breaks = seq(0, max(lfa$age) + 12, by = 12)
                                , labels = paste(
                                                   paste0(seq(0, max(lfa$age) + 12, by = 12), "m")
                                                 , paste0(seq(0, max(lfa$age) + 12, by = 12) / 12, "yr")
                                                 , sep = "\n")
                                ) +
    ggplot2::scale_y_continuous(name = "Length or Height (cm)") +
    ggplot2::scale_color_hue(name = "Length for Age percentile") +
    ggplot2::theme(
                   legend.position = "bottom"
                   )
}
#'
#'
#+ label = "lfa_female", echo = FALSE, fig.width = 8, fig.height = 6
g(lfa[male == "Female"])
#'
#'
#+ label = "lfa_male", echo = FALSE, fig.width = 8, fig.height = 6
g(lfa[male == "Male"])
#'
#' ## Weight for Age
#'
#' There are two methods for determining weight for age both based on CDC
#' National Center for Health Statistics data: one for infants (weighed laying
#' flat) up to 36 months [@cdc_weight_for_age_0_36],
#' and one for children (weighed on a standing scale) over 24 months
#' [@cdc_weight_for_age].
#'
#' A 33 pound (
{{ round(33 * 0.453592, 2)}}
#' kg) 4 year old male is in the
{{ paste0(round(100 * p_weight_for_age(33 * 0.453592, age = 4 * 12, male = 1), 2), 'th') }}
#' percentile.
p_weight_for_age(33 * 0.453592, age = 4 * 12, male = 1)
#'
#' The 20th percentile weight for an 18 month old infant female is
{{ round(q_weight_for_age(p = 0.2, age = 18, male = 0), 5)}}
#' kg.
#+
round(q_weight_for_age(p = 0.2, age = 18, male = 0), 5)
#'
#' ## Weight for Length or Stature
#'
#' Similar to weight-for-age, there are two methods for determining
#' weight-for-length.  Both methods utilize data from the CDC National Center for
#' Health Statistics.  The first method is used for infants under 36 months
#' (used for children who are measured laying flat), and the second is used for
#' child over 24 months (used for children able to be measured while standing
#' up).  The overlapping range between the methods will differ.
#'
#' The median weight for a male, 95 cm long infant
{{ q_weight_for_stature(p = 0.5, stature = 95, male = 1) }}
#' kg, whereas the median weight for a 95 cm tall child is
{{ q_weight_for_stature(p = 0.5, stature = 95, male = 1) }}
#' kg.
#'
q_weight_for_stature(p = 0.5, stature = 95, male = 1)
q_weight_for_stature(p = 0.5, stature = 95, male = 1)
#'
#' A 5.8 kg, 61 cm long female infant is in the
{{ p_weight_for_stature(q = 5.8, stature = 61, male = 0) }}
#' weight percentile.
p_weight_for_stature(q = 5.8, stature = 61, male = 0)
#'
#' ## BMI for Age
#'
#' For a twelve year old, a BMI of 22.2 corresponds to the
{{ paste0(round(p_bmi_for_age(q = 22.2, age = 144, male = 0) * 100, 2), "th") }}
#' BMI percentile for a female, and the
{{ paste0(round(p_bmi_for_age(q = 22.2, age = 144, male = 1) * 100, 2), "th") }}
#' BMI percentile for a male.
#+
p_bmi_for_age(q = 22.2, age = c(144, 144), male = c(0, 1))
#'
#' The median BMI values for a 10 year old male and females are:
q_bmi_for_age(p = 0.5, age = c(120, 120), male = c(1, 0))
#'
#' ## Head Circumference
#'
#' A 10 month old male has a median head circumference of
{{ q_head_circ_for_age(p = 0.5, age = 10, male = 1) }}
#' cm.
#'
#' A head circumference of 42 cm for an 8 month old female is in the
{{ paste0(round(100*p_head_circ_for_age(q = 42, age = 8, male = 0), 5), "th")}}
#' percentile.
#'
q_head_circ_for_age(p = 0.5, age = 10, male = 1)
p_head_circ_for_age(q = 42, age = 8, male = 0)
#'
#'
#' # Additional Utilities
#'
#' ## Estimating Gaussian Mean and Standard Deviation
#'
#' The NHLBI data for blood pressures provided values in percentiles.  To get a
#' mean and standard deviation that would work well for estimating other
#' percentiles and quantiles via a Gaussian distribution we optimized for values
#' of the mean and standard deviation such that for the provided quantiles $q_i$
#' at the $p_i$ percentiles and $X \sim N\left(\mu, \sigma\right)$,
#'
#' $$ \sum_{i} \left(\Pr(X \leq q_i) - p_i \right)^2, $$
#'
#' was minimized.  The NHLBI data is provided to the end user.
data(list = "nhlbi_bp_norms", package = "pedbp")
str(nhlbi_bp_norms)
#'
#' For an example of how we fitted the parameters:
d <- nhlbi_bp_norms[nhlbi_bp_norms$age == 144 & nhlbi_bp_norms$height_percentile == 50, ]
d <- d[d$male == 0, ]
d

est_norm(q = d$sbp, p = d$bp_percentile / 100)
est_norm(q = d$dbp, p = d$bp_percentile / 100)

bp_parameters[bp_parameters$male == 0 & bp_parameters$age == 144 & bp_parameters$height_percentile == 50, ]

#'
#' The est_norm method comes with a plotting method too.  The provided quantiles
#' are plotted as open dots and the fitted distribution function is plotted to
#' show the fit.
#+ fig.width = 5, fig.height = 5
plot( est_norm(q = d$dbp, p = d$bp_percentile / 100) )

#'
#' If you want to emphasize a data point you can do that as well.  Here is an
#' example from a set of quantiles and percentiles which are not Gaussian.
#'
qs <- c(-1.92, 0.05, 0.1, 1.89) * 1.8 + 3.14
ps <- c(0.025, 0.40, 0.50, 0.975)

# with equal weights
w0 <- est_norm(qs, ps)
# weight to ignore one of the middle value and make sure to hit the other
w1 <- est_norm(qs, ps, weights = c(1, 2, 0, 1))
# equal weight the middle, more than the tails
w2 <- est_norm(qs, ps, weights = c(1, 2, 2, 1))

#'
#'
#+ label = "est_norm_plots", fig.width = 9, fig.height = 5
gridExtra::grid.arrange(
  plot(w0) + ggplot2::ggtitle(label = "w0", subtitle = paste0("Mean: ", round(w0$par[1], 2), " SD: ", round(w0$par[2], 3)))
  , plot(w1) + ggplot2::ggtitle(label = "w1", subtitle = paste0("Mean: ", round(w1$par[1], 2), " SD: ", round(w1$par[2], 3)))
  , plot(w2) + ggplot2::ggtitle(label = "w2", subtitle = paste0("Mean: ", round(w2$par[1], 2), " SD: ", round(w2$par[2], 3)))
  , nrow = 1
)

#'
#'
#'
#' # References
#'<div id="refs"></div>
#'
#' # Session Info
#+ label = "sessioninfo"
sessionInfo()
