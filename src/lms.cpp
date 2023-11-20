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

  // use a binary search to get the row of LUT needed
  if (x < min(LUT.col(0))) {
    Rf_error("age/stature below lower limit");
  } else if (x > max(LUT.col(0))) {
    Rf_error("age/stature above upper limie");
  }

  int l=0, r = LUT.n_rows - 1, m;
  unsigned int i = 0;
  while(l < r) {
    m = l + (r - l) / 2;
    if (LUT.col(0)(m) < x) {
      l = m;
    } else {
      r = m - 1;
    }
    ++i;
    if (i > LUT.n_rows) {
      Rf_error("somthing very bad has happended.");
    }
  }

  return LUT.row(l);
}


