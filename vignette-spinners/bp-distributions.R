#'---
#'title: "Pediatric Blood Pressure Distributions"
#'output:
#'  bookdown::html_document2:
#'    toc: true
#'    number_sections: true
#'bibliography: references.bib
#'vignette: >
#'  %\VignetteIndexEntry{Pediatric Blood Pressure Distributions}
#'  %\VignetteEngine{knitr::rmarkdown}
#'  %\VignetteEncoding{UTF-8}
#'---
#'
#+ label = "setup", include = FALSE
knitr::opts_chunk$set(collapse = TRUE)
#'
#' # Introduction
#'
#' Part of the work of @martin2022machine required transforming blood
#' pressurement measurement into percentiles based on published norms.  This
#' work was complicated by the fact that data for pediatric blood pressure
#' precentiles is sparse and gennerally only applicable to children at least
#' one year of age and requires height, a commonly unavailable data point in
#' electronic health records for a variety of reasons.
#'
#' A solution to building pediatric blood pressure percentiles was developed and
#' is presented here for others to use.  Inputs for the developed method are:
#'
#' 1. Patient sex (male/female) _required_
#' 2. Systolic blood pressure (mmHg) _required_
#' 3. Diastolic blood pressure (mmHg) _required_
#' 4. Patient height (cm) _if known_.
#'
#' Given the inputs the following logic is used to determine which data sets
#' will be used to inform the blood pressure percentiles.  Under one year of
#' age, the data from @gemelli1990longitudinal will be used; height is
#' irrelevent.  For those at least one year of age with a known height then the
#' @flynn2017clinical data sets are used.  If height is unknown and age is at
#' least three years then data from @lo2013prehypertension is used.  Lastly,
#' under three years of age with unknown height have blood pressure precentiles
#' estimated by the @flynn2017clinical data with the default of the median
#' height for sex and age (Figure \@ref(fig:flowchart)).
#'
#' ![](./flowchart.png)
#+ label = "flowchart", echo = FALSE, results = "hide", out.width = 0.1, out.height = 0.1, fig.cap = "Flowchart for determining which data source informs blood pressure percentiles by age and height."
plot(1:10) # this is just here to "trick" a figure caption onto the flowchart
#'
#' # References
#'<div id="refs"></div>
#'
#' # Session Info
#+ label = "sessioninfo"
sessionInfo()
