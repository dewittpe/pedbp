#'---
#'title: "Additional Utilities"
#'output:
#'  rmarkdown::html_vignette:
#'    toc: true
#'    number_sections: true
#'bibliography: references.bib
#'vignette: >
#'  %\VignetteIndexEntry{Additional Utilities}
#'  %\VignetteEngine{knitr::rmarkdown}
#'  %\VignetteEncoding{UTF-8}
#'---
#'
#+ label = "setup", include = FALSE
#/*
devtools::load_all() # load the dev version while editting
#*/
################################################################################
#                        !!! DO NOT EDIT .Rmd files !!!                        #
#                                                                              #
# .Rmd files are generated by their corresponding .R files found in the        #
# vignette-spinners/ directory.  Any changes needed to the .Rmd file need to   #
# be made in the .R file                                                       #
################################################################################
knitr::opts_chunk$set(collapse = TRUE, fig.align = "center")
library(pedbp)

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
