// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <Rmath.h>
#include "growth_standards.h"
#include "lms_data.h"
#include "utilities.h"

// -------------------------------------------------------------------------- //
// cpp Pediatric Growth Standardards Function 1
//
// Find the percentile, quantile, or z-score for a growth standard given the
// inputs
//
// args:
//   metric bmi_for_age, length_for_age, height_for_age, weight_for_length,
//          weight_for_height, ...
//   source CDC or WHO
//   male   0 = female; 1 = male
//   x      the age, length, or height; as needed for the metric ---\  qp_for_x
//   qp     the quantile or percentile for bmi, length,...       ---/  qp_for_x
//   type   one of 'quantile', 'distribution' (for percentiles), or 'zscore',
//          the value of type defines the output
//
// return:
//   a double; the quantile, percentile (distribution value), or zscore (as
//   defined by the value of the input 'type').
//
double cppPGSF1(std::string metric, std::string source, int male, double x, double qp, std::string type) {
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
  } else if (metric == "head_circumference_for_age") {
    if (source == "WHO") {
      if (male == 1) {
        LUT = head_circumference_for_age_who_male();
      } else {
        LUT = head_circumference_for_age_who_female();
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
  } else if (metric == "height_for_age") {
    if (source == "WHO") {
      if (male == 1) {
        LUT = height_for_age_who_male();
      } else {
        LUT = height_for_age_who_female();
      }
    } else if (source == "CDC") {
      if (male == 1) {
        LUT = height_for_age_cdc_male();
      } else {
        LUT = height_for_age_cdc_female();
      }
    } else {
      Rf_error("Unknown source for height_for_age data");
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
  } else if (metric == "weight_for_height") {
    if (source == "WHO") {
      if (male == 1) {
        LUT = weight_for_height_who_male();
      } else {
        LUT = weight_for_height_who_female();
      }
    } else if (source == "CDC") {
      if (male == 1) {
        LUT = weight_for_height_cdc_male();
      } else {
        LUT = weight_for_height_cdc_female();
      }
    } else {
      Rf_error("Unknown source for weight_for_height data");
    }
  } else if (metric == "length_for_age") {
    if (source == "WHO") {
      if (male == 1) {
        LUT = length_for_age_who_male();
      } else {
        LUT = length_for_age_who_female();
      }
    } else if (source == "CDC") {
      if (male == 1) {
        LUT = length_for_age_cdc_male();
      } else {
        LUT = length_for_age_cdc_female();
      }
    } else {
      Rf_error("Unknown source for length_for_age data");
    }
  } else if (metric == "weight_for_length") {
    if (source == "WHO") {
      if (male == 1) {
        LUT = weight_for_length_who_male();
      } else {
        LUT = weight_for_length_who_female();
      }
    } else if (source == "CDC") {
      if (male == 1) {
        LUT = weight_for_length_cdc_male();
      } else {
        LUT = weight_for_length_cdc_female();
      }
    } else {
      Rf_error("Unknown source for weight_for_length data");
    }
  } else {
    Rf_error("Unknown metric");
  }

  // use a binary search to get the row of LUT needed
  int left=0, right = LUT.n_rows - 1, mid;
  unsigned int i = 0;

  if (x < min(LUT.col(0))) {
    Rf_warning("age/stature below lower limit");
    return R_NaN;
  } else if (x > max(LUT.col(0))) {
    Rf_warning("age/stature above upper limit");
    return R_NaN;
  }

  while(left < right) {

    mid = left + ((right - left) / 2);

    // Useful for debugging
    //Rcpp::Rcout << "i: " << i << "\n";
    //Rcpp::Rcout << "left: " << left << "\n";
    //Rcpp::Rcout << "mid: " << mid << "\n";
    //Rcpp::Rcout << "right: " << right << "\n";
    //Rcpp::Rcout << "LUT.col(0)(left)" << LUT.col(0)(left) << "\n";
    //Rcpp::Rcout << "LUT.col(0)(mid)" << LUT.col(0)(mid) << "\n";
    //Rcpp::Rcout << "LUT.col(0)(right)" << LUT.col(0)(right) << "\n\n";

    if (std::abs(LUT.col(0)(left) - x) < 0.00000001) {
      right = left;
      mid = left;
    } else if (std::abs(LUT.col(0)(mid) - x)  < 0.00000001) {
      left = mid;
      right = mid;
    } else if (std::abs(LUT.col(0)(right) - x) < 0.0000001) {
      left = right;
      mid = right;
    } else if (left == mid) {
      right = mid;
    } else if (LUT.col(0)(mid) < x) {
      left = mid;
    } else {
      right = mid;
    }

    ++i;
    if (i > LUT.n_rows) {
      Rf_error("something very bad has happended - a binary search when n steps.");
    }
  }

  // get the p, q, or z-score
  double z,l,m,s;
  l = LUT.col(1)(left);
  m = LUT.col(2)(left);
  s = LUT.col(3)(left);

  //Rcpp::Rcout << "LUT.col(0)(left)" << LUT.col(0)(left) << "\n";
  //Rcpp::Rcout << "LUT.col(1)(left)" << LUT.col(1)(left) << "\n";
  //Rcpp::Rcout << "LUT.col(2)(left)" << LUT.col(2)(left) << "\n";
  //Rcpp::Rcout << "LUT.col(3)(left)" << LUT.col(3)(left) << "\n";
  //Rcpp::Rcout << "l: " << l << "\n";
  //Rcpp::Rcout << "m: " << m << "\n";
  //Rcpp::Rcout << "s: " << s << "\n";

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
    } else if (type == "distribution") {
      // distribution value
      return R::pnorm(z, 0, 1, 1, 0);
    } else {
      Rf_error("type needs to be one of 'quantile', 'distribution', or 'zscore'");
    }
  }
}

//' @title Pediatric Growth Standards
//'
//' @description Pediatric growth standard based on LMS data from the CDC and WHO.
//'
//' @details expect to call this from R after checking some functional
//' arguments within R.
//'
//' @param metric string, for example bmi_for_age
//' @param source string, CDC or WHO
//' @param male  integer, 0 = female; 1 = male
//' @param x is the age (in months), length (cm) or height (cm) as needed for
//' the metric.
//' @param qp the quantile or percentile, whichever is relevant for the type
//' @param type quantile, distribution, or zscore
//'
// [[Rcpp::export]]
Rcpp::NumericVector cppPGSF(
    Rcpp::CharacterVector metric,
    Rcpp::CharacterVector source,
    Rcpp::IntegerVector male,
    Rcpp::NumericVector x,
    Rcpp::NumericVector qp,
    Rcpp::CharacterVector type
    )
{
  // vector length - either length 1, or equal length.
  int max_length = std::max({metric.length(), source.length(), male.length(), x.length(), qp.length(), type.length()});
  int min_length = std::min({metric.length(), source.length(), male.length(), x.length(), qp.length(), type.length()});

  if (min_length == 0) {
    Rf_error("zero length vector");
  }

  if (max_length > 1) {

    if (
        (metric.length() > 1 && metric.length() < max_length) ||
        (source.length() > 1 && source.length() < max_length) ||
        (male.length()   > 1 && male.length()   < max_length) ||
        (x.length()      > 1 && x.length()      < max_length) ||
        (qp.length()     > 1 && qp.length()     < max_length) ||
        (type.length()   > 1 && type.length()   < max_length)
       ) {
      Rf_error("all input vectors need to be of equal length, or length 1.");
    }

    if (metric.length() == 1) {
      metric = resize(metric, max_length);
      metric.fill(metric(0));
    }

    if (source.length() == 1) {
      source = resize(source, max_length);
      source.fill(source(0));
    }

    if (male.length() == 1) {
      male = resize(male, max_length);
      male.fill(male(0));
    }

    if (x.length() == 1) {
      x = resize(x, max_length);
      x.fill(x(0));
    }

    if (qp.length() == 1) {
      qp = resize(qp, max_length);
      qp.fill(qp(0));
    }

    if (type.length() == 1) {
      type = resize(type, max_length);
      type.fill(type(0));
    }

  }

  Rcpp::NumericVector rtn (max_length);
  for(int i = 0; i < max_length; ++i) {
    rtn(i) = cppPGSF1(Rcpp::as<std::string>(metric(i)), Rcpp::as<std::string>(source(i)), male(i), x(i), qp(i), Rcpp::as<std::string>(type(i)));
  }
  return rtn;
}
// -------------------------------------------------------------------------- //
//                                End of File                                 //
// -------------------------------------------------------------------------- //
