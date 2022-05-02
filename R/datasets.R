#' Data Sets Informing Blood Pressure Percentile Estimates
#'
#' A collection of data sets from multiple sources used to inform blood pressure
#' percentiles for pediatrics patients by sex, age, and height (if known).
#'
#' Data sets are named to reflect the source.
#'
#' For all the data sets provided units are uniform:
#'
#' \describe{
#'    \item{age:}{Patient age; months}
#'    \item{height:}{length/height/stature; cm}
#'    \item{male:}{integer value; 1 = male, 0 = female}
#'    \item{sbp:}{systolic blood pressure; mmHg}
#'    \item{dbp:}{diastolic blood pressure; mmHg}
#' }
#'
#' Columns with a name such as \code{sbp} is a point observations.  Summary
#' statistics are appended to the variable as needed, e.g., \code{sbp_mean} and
#' \code{sbp_sd} for the reported mean and standard deviation of systolic blood
#' pressue.
#'
#' CDC ages represent whole month but reported at the half month.  That is, age
#' = 12.5 is short-hand for 12 <= age < 13. The exception is birth; age = 0 is
#' birth and not a range.
#'
#' \code{bp_parameters} has the estimated mean and
#' standard deviations for estimating percentiles using a Gaussian distribution
#' for a given sex, age (in months), and height (if known/applicable).
#'
#' @seealso \code{vignette("bp-distributions", package = "pedbp")}
#'
#' @references
#'
#' Expert Panel on Integrated Guidelines for Cardiovascular Health and Risk
#' Reduction in Children and Adolescents. "Expert panel on integrated guidelines
#' for cardiovascular health and risk reduction in children and adolescents:
#' summary report." Pediatrics 128.Supplement_5 (2011): S213-S256.
#'
#' Gemelli, M., Manganaro, R., Mami, C., & De Luca, F. (1990). Longitudinal
#' study of blood pressure during the 1st year of life. European journal of
#' pediatrics, 149(5), 318-320.
#'
#' Lo, Joan C., et al. "Prehypertension and hypertension in community-based
#' pediatric practice." Pediatrics 131.2 (2013): e415-e424.
#'
#'
#' @name bpdata
#' @keywords datasets
NULL

#' @rdname bpdata
"lo2013"

#' @rdname bpdata
"gemelli1990"

#' @rdname bpdata
"nhlbi_bp_norms"

#' @rdname bpdata
"bp_parameters"

