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
arma::mat lms_bmi() {
  return bmi_for_age_who_male();
}
