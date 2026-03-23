#' PEDBP
#'
#' Tools for pediatric blood pressure distributions and growth standards.
#'
#' @section Notation:
#' Across the package, arguments named \code{p} or beginning with \code{p_}
#' denote probabilities on the 0 to 1 scale. Arguments or values named
#' \code{percentile} denote percentile points on the 0 to 100 scale.
#'
#' For example, use \code{p_sbp = 0.85} for the 85th percentile as a probability
#' input, and use \code{height_percentile = 85} for the 85th height percentile
#' as percentile points.
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @useDynLib pedbp
#' @importFrom Rcpp sourceCpp
## usethis namespace: end
NULL
