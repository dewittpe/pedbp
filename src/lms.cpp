// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <Rmath.h>
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
double cppPGSF(std::string metric, std::string source, int male, double x, double qp, std::string type) {
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
    Rf_warning("age/stature below lower limit");
  } else if (x > max(LUT.col(0))) {
    Rf_warning("age/stature above upper limit");
  }

  int left=0, right = LUT.n_rows - 1, mid;
  unsigned int i = 0;
  while(left < right) {

    mid = left + ((right - left) / 2);

    //Rcpp::Rcout << "i" << i << "\n";
    //Rcpp::Rcout << "left" << left << "\n";
    //Rcpp::Rcout << "right" << right << "\n";
    //Rcpp::Rcout << "mid" << mid << "\n";
    //Rcpp::Rcout << "LUT.col(0)(left)" << LUT.col(0)(left) << "\n";
    //Rcpp::Rcout << "LUT.col(0)(mid)" << LUT.col(0)(mid) << "\n";
    //Rcpp::Rcout << "LUT.col(0)(right)" << LUT.col(0)(right) << "\n";
    //Rcpp::Rcout << "x" << x << "\n";

    //if (LUT.col(0)(mid) == x) {
    if (abs(LUT.col(0)(mid) - x) < 0.000001) {
      //Rcpp::Rcout << "equality\n";
      left = mid;
      right = mid;
    } else if (abs(LUT.col(0)(right) - x) < 0.000001) {
      left = right;
      mid = right;
    } else if (LUT.col(0)(mid) < x) {
      //Rcpp::Rcout << "left\n";
      left = mid;
    } else {
      //Rcpp::Rcout << "right\n";
      right = mid - 1;
    }
    ++i;
    if (i > LUT.n_rows) {
      Rf_error("somthing very bad has happended.");
    }
  }

  // get the p, q, or z-score
  double z,l,m,s;
  l = LUT.col(1)(left);
  m = LUT.col(2)(left);
  s = LUT.col(3)(left);

  if (type == "quantile") {
    z = R::qnorm(qp, 0, 1, 1, 0);
    if (l == 0) {
      return m * exp(s * z);
    } else {
      return m * pow(1.0 + l * s * z,   1.0 / l );
    }
  } else {
    // zscore
    if (l == 0) {
      z = log(qp / m) / s;
    } else {
      z = ( pow( qp / m, l) - 1.0) / (l * s);
    }
    if (type == "zscore") {
      return z;
    } else {
      // distribution value
      return R::pnorm(z, 0, 1, 1, 0);
    }
  }

  //return LUT.row(l);
  //return z;
}


