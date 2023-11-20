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

  if (!(male == 0 || male == 1)) {
    Rf_error("male needs to be a 0 or 1");
  }

  if (metric == "bmi_for_age") {
    if (source == "WHO") {
      if (male == 1) {
        LUT = bmi_for_age_who_male();
      } else {
        LUT = bmi_for_age_who_female();
      }
    } else if (source == "CDC") {
      if (male == 1) {
        LUT = bmi_for_age_cdc_male();
      } else {
        LUT = bmi_for_age_cdc_female();
      }
    } else {
      Rf_error("Unknown source for bmi_for_age data");
    }
  } else if (metric == "head_circumference_for_age_") {
    if (source == "WHO") {
      if (male == 1) {
        //LUT = head_circumference_for_age_who_male();
        Rf_warning("head circumference for age who male not yet defined");
      } else {
        //LUT = head_circumference_for_age_who_female();
        Rf_warning("head circumference for age who female not yet defined");
      }
    } else if (source == "CDC") {
      if (male == 1) {
        LUT = head_circumference_for_age_cdc_male();
      } else {
        LUT = head_circumference_for_age_cdc_female();
      }
    } else {
      Rf_error("Unknown source for head_circumference_for_age data");
    }
  } else if (metric == "stature_for_age") {
    if (source == "WHO") {
      if (male == 1) {
        LUT = stature_for_age_who_male();
      } else {
        LUT = stature_for_age_who_female();
      }
    } else if (source == "CDC") {
      if (male == 1) {
        LUT = stature_for_age_cdc_male();
      } else {
        LUT = stature_for_age_cdc_female();
      }
    } else {
      Rf_error("Unknown source for stature_for_age data");
    }
  } else if (metric == "weight_for_age") {
    if (source == "WHO") {
      if (male == 1) {
        LUT = weight_for_age_who_male();
      } else {
        LUT = weight_for_age_who_female();
      }
    } else if (source == "CDC") {
      if (male == 1) {
        LUT = weight_for_age_cdc_male();
      } else {
        LUT = weight_for_age_cdc_female();
      }
    } else {
      Rf_error("Unknown source for weight_for_age data");
    }
  } else if (metric == "weight_for_stature") {
    if (source == "WHO") {
      if (male == 1) {
        LUT = weight_for_stature_who_male();
      } else {
        LUT = weight_for_stature_who_female();
      }
    } else if (source == "CDC") {
      if (male == 1) {
        LUT = weight_for_stature_cdc_male();
      } else {
      }
        LUT = weight_for_stature_cdc_female();
    } else {
      Rf_error("Unknown source for weight_for_stature data");
    }
  } else {
    Rf_error("Unknown metric");
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


