// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include "lms.h"

//' @title LMS data and distribution estimates
//'
//' @param metric
//' @param male
//' @param source
//' @param age (in months)
//' @param stature (in cm)
//'
//' @export
// [[Rcpp::export]]
arma::mat cppPGSF(std::string metric, std::string source, int male, double x, double qp, std::string type) {
  // get the needed look up table
  arma::mat LUT;

  if (metric == "bmi_for_age") {
    if (source == "WHO") {
      if (male == 1) {
        LUT = bmi_for_age_who_male();
      } else if (male == 0) {
        LUT = bmi_for_age_who_female();
      }
    } else if (source == "CDC") {
      if (male == 1) {
        LUT = bmi_for_age_cdc_male();
      } else if (male == 0) {
        LUT = bmi_for_age_cdc_female();
      }
    }
  } else {
    Rf_error("Not yet implemented");
  }

  return LUT;
}


