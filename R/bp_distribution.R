#' Estimate Pediatric Blood Pressure Distribution
#'
#' Percentile and quantile functions for pediatric blood pressure.
#'
#' \code{source} is used to specify the method or source data sets by which the
#' percentiles are generated.  This can be controlled by the option
#' \code{pedbp_bp_source}.
#' End users are encouraged to set the option if not using the default so all
#' calls to these functions will use the same source.
#'
#' Options:
#' \itemize{
#'   \item \code{martin2022} (default) uses a combination of references to generate
#'   percentiles ages from 1 months through 18 years, without or without known
#'   stature.  This was the only method implemented in version 1 of the pedbp package.
#'
#'   \item \code{gemelli1990} uses only the reference values from Gemelli et al.
#'   (1990).  These values are applicable to patients from 1 month to 12 months
#'   of age. Stature is not used in the look up for the parameters.
#'
#'   \item \code{lo2013} uses only the reference values from Lo et al. (2013).
#'   This is applicable to patients of at least three years of age.  Height is
#'   not considered when looking up the parameters.
#'
#'   \item \code{nhlbi} uses only reference values from the National Heart,
#'   Lung, and Blood Institute [NHLBI] and the Centers for Disease Control and
#'   Prevention [CDC] published in 2011.  These are for patients of at least one
#'   year of age and with a known stature.  These values were publish
#'
#'   \item \code{flynn2017} uses only reference values from Flynn et al. (2017).
#'   These values are similar to the nhlbi values _but_ "do not include children
#'   and adolescents with overweight and obesity (ie, those with a BMI >= 85th
#'   percentile).
#' }
#'
#'
#' @param q_sbp a vector of systolic blood pressures
#' @param q_dbp a vector of diastolic blood pressures
#' @param p_sbp a vector of systolic blood percentiles
#' @param p_dbp a vector of diastolic blood percentiles
#' @param age numeric age, in months
#' @param male integer value, 1 = male, 0 = female
#' @param height numeric, in centimeters, can be missing. See Details.
#' @param height_percentile height percentile to use. See Details.
#' @param default_height_percentile default height percentile to use if \code{height} is
#' missing.
#' @param source the method, or data set, to use as the reference.  See Details.
#' @param ... not currently used
#'
#' @seealso \code{vignette("bp-distriution", package = "pedbp")}
#'
#' @return a \code{pedbp_bp} object.  This is a list of two numeric vectors:
#' \code{sbp_percentile} (systolic blood pressure) and \code{dbp_percentile}
#' (diastolic blood pressure).  Additionally, the \code{bp_params} attribute
#' provides details on the data source and parameters used in the percentile
#' estimates.
#'
#' @references
#'
#' Gemelli, Marina, Rosa Manganaro, Carmelo Mamì, and F. De Luca. "Longitudinal
#' study of blood pressure during the 1st year of life." European journal of
#' pediatrics 149 (1990): 318-320.
#'
#' Lo, Joan C., Alan Sinaiko, Malini Chandra, Matthew F. Daley, Louise C.
#' Greenspan, Emily D. Parker, Elyse O. Kharbanda et al. "Prehypertension and
#' hypertension in community-based pediatric practice." Pediatrics 131, no. 2
#' (2013): e415-e424.
#'
#' "Expert panel on integrated guidelines for cardiovascular health and risk
#' reduction in children and adolescents: summary report." Pediatrics 128, no.
#' Suppl 5 (2011): S213. <doi:10.1542/peds.2009-2107C>
#'
#' The Fourth Report on the Diagnosis, Evaluation, and Treatment of High Blood
#' Pressure in Children and Adolescents National High Blood Pressure Education
#' Program Working Group on High Blood Pressure in Children and Adolescents
#' Pediatrics 2004;114;555-576 <doi:10.1542/peds.114.2.S2.555>
#'
#' Flynn, Joseph T., David C. Kaelber, Carissa M. Baker-Smith, Douglas Blowey,
#' Aaron E. Carroll, Stephen R. Daniels, Sarah D. De Ferranti et al. "Clinical
#' practice guideline for screening and management of high blood pressure in
#' children and adolescents." Pediatrics 140, no. 3 (2017).
#'
#' @examples
#'
#' x <- p_bp(q_sbp = 100, q_dbp = 60, age = 8, male = 0)
#' x
#' str(x)
#'
#' x <- p_bp(q_sbp = c(NA, 82), q_dbp = c(60, 72), age = 9.2, male = 0)
#' x
#' str(x)
#'
#' x <- p_bp(q_sbp = c(NA, 82), q_dbp = c(60, 72), age = 29.2, male = 0, height = 82.8)
#' x
#' str(x)
#'
#' x <- q_bp(p_sbp = 0.78, p_dbp = 0.65, age = 8, male = 0)
#' x
#' str(x)
#'
#' #############################################################################
#' # compare results when height is known or unknown
#' p_bp(q_sbp = rep(100, 2),
#'      q_dbp = rep( 60, 2),
#'      age   = rep(35.75, 2),
#'      male  = c(0, 0),
#'      height = c(NA, 100))
#'
#' #############################################################################
#' # Working with multiple patients records
#' d <- read.csv(system.file("example_data", "for_batch.csv", package = "pedbp"))
#' d
#'
#' bp_percentiles <-
#'   p_bp(
#'       q_sbp = d$sbp..mmHg.
#'     , q_dbp = d$dbp..mmHg.
#'     , age   = d$age_months
#'     , male  = d$male
#'     )
#' bp_percentiles
#'
#' q_bp(
#'     p_sbp = bp_percentiles$sbp_percentile
#'   , p_dbp = bp_percentiles$dbp_percentile
#'   , age   = d$age_months
#'   , male  = d$male
#'   )
#'
#' #############################################################################
#' # Selecting different source values
#'
#' # default
#' p_bp(q_sbp = 92, q_dbp = 60, age = 29.2, male = 0, default_height_percentile = 0.95,
#'      source = "martin2022")
#' p_bp(q_sbp = 92, q_dbp = 60, age = 29.2, male = 0, default_height_percentile = 0.95,
#'      source = "gemelli1990")
#' p_bp(q_sbp = 92, q_dbp = 60, age = 29.2, male = 0, default_height_percentile = 0.95,
#'      source = "lo2013")
#' p_bp(q_sbp = 92, q_dbp = 60, age = 29.2, male = 0, default_height_percentile = 0.95,
#'      source = "nhlbi")
#' p_bp(q_sbp = 92, q_dbp = 60, age = 29.2, male = 0, default_height_percentile = 0.95,
#'      source = "flynn2017")
#'
#' q_bp(p_sbp = 0.85, p_dbp = 0.85, age = 29.2, male = 0, default_height_percentile = 0.95,
#'      source = "martin2022") # default
#' q_bp(p_sbp = 0.85, p_dbp = 0.85, age = 29.2, male = 0, default_height_percentile = 0.95,
#'      source = "gemelli1990")
#' q_bp(p_sbp = 0.85, p_dbp = 0.85, age = 29.2, male = 0, default_height_percentile = 0.95,
#'      source = "lo2013")
#' q_bp(p_sbp = 0.85, p_dbp = 0.85, age = 29.2, male = 0, default_height_percentile = 0.95,
#'      source = "nhlbi")
#' q_bp(p_sbp = 0.85, p_dbp = 0.85, age = 29.2, male = 0, default_height_percentile = 0.95,
#'      source = "flynn2017")
#'
#'
#' @name bp_distribution
NULL

#' @rdname bp_distribution
#' @export
p_bp <- function(q_sbp, q_dbp, age, male, height = NA, height_percentile = NA, default_height_percentile = 0.50, source = getOption("pedbp_bp_source", "martin2022"), ...) {
  source <- match.arg(source, choices = c("martin2022", "gemelli1990", "nhlbi", "lo2013", "flynn2017"), several.ok = FALSE)
  rtn <- cppBP(qp_sbp = q_sbp, qp_dbp = q_dbp, age = age, male = male, height = height, height_percentile = height_percentile, default_height_percentile = default_height_percentile, source = source, type = "percentile")
  class(rtn) <- c("pedbp_bp", "pedbp_p_bp")
  rtn
}

#' @rdname bp_distribution
#' @export
q_bp <- function(p_sbp, p_dbp, age, male, height = NA, height_percentile = NA, default_height_percentile = 0.50, source = getOption("pedbp_bp_source", "martin2022"), ...) {
  source <- match.arg(source, choices = c("martin2022", "gemelli1990", "nhlbi", "lo2013", "flynn2017"), several.ok = FALSE)
  rtn <- cppBP(qp_sbp = p_sbp, qp_dbp = p_dbp, age = age, male = male, height = height, height_percentile = height_percentile, default_height_percentile = default_height_percentile, source = source, type = "quantile")
  class(rtn) <- c("pedbp_bp", "pedbp_q_bp")
  rtn
}

#' @export
print.pedbp_bp <- function(x, ...) {
  print(x[1:2])
  invisible(x)
}
