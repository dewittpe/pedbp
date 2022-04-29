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
#' statistics are prepended to the variable as needed, e.g., \code{mean_sbp} and
#' \code{sd_sbp} for the reported mean and standard deviation of systolic blood
#' pressue.
#'
#' CDC ages represent whole month but reported at the half month.  That is, age
#' = 12.5 is short-hand for 12 <= age < 13. The exception is birth; age = 0 is
#' birth and not a range.
#'
#' @seealso \code{vignette("bp-distributions", package = "pedbp")}
#'
#' @references
#'
#' Flynn JT, Kaelber DC, Baker-Smith CM, et al. Clinical Practice Guideline for
#' Screening and Management of High Blood Pressure in Children and Adolescents.
#' Pediatrics. 2017;140(3):e20171904
#'
#' Lo, Joan C., et al. "Prehypertension and hypertension in community-based
#' pediatric practice." Pediatrics 131.2 (2013): e415-e424.
#'
#' @name bpdata
#' @keywords datasets
NULL

#' @rdname bpdata
"lo2013"

#' @rdname bpdata
"gemelli1990"

#' @rdname bpdata
"cdc_length_for_age"
