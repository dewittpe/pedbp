// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <Rmath.h>
#include "lms_data.h"
#include "bp_params.h"

using namespace Rcpp;

// -------------------------------------------------------------------------- //
// Utility functions
Rcpp::NumericVector resize(Rcpp::NumericVector x, int length) {
  Rcpp::NumericVector rtn(length);
  for ( int i = 0; i < x.size(); ++i ) {
    rtn[i] = x(i);
  }
  return rtn;
}

Rcpp::CharacterVector resize(Rcpp::CharacterVector x, int length) {
  Rcpp::CharacterVector rtn(length);
  for ( int i = 0; i < x.size(); ++i ) {
    rtn[i] = x(i);
  }
  return rtn;
}

Rcpp::IntegerVector resize(Rcpp::IntegerVector x, int length) {
  Rcpp::IntegerVector rtn(length);
  for ( int i = 0; i < x.size(); ++i ) {
    rtn[i] = x(i);
  }
  return rtn;
}

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
//   type   one of 'quantile', 'percentile', or 'zscore', the value of type
//          defines the output
//
// return:
//   a double; the quantile, percentile, or zscore (as defined by the value
//   of the input 'type').
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
        //LUT = head_circumference_for_age_who_male();
        Rf_error("head circumference for age WHO male not yet defined");
      } else {
        //LUT = head_circumference_for_age_who_female();
        Rf_error("head circumference for age WHO female not yet defined");
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
    } else {
      // distribution value
      return R::pnorm(z, 0, 1, 1, 0);
    }
  }
}

// -------------------------------------------------------------------------- //
//
// height is 1 for known and 0 for unknown
//
// [[Rcpp::export]]
Rcpp::NumericVector cppBPF1(double sbp, double dbp, double age, int male, int
    known_height, double height_percentile, std::string source, std::string
    type) {
  arma::mat LUT;

  Rcpp::Rcout << "sbp: " << sbp << "\n";
  Rcpp::Rcout << "dbp: " << dbp << "\n";
  Rcpp::Rcout << "age: " << age << "\n";
  Rcpp::Rcout << "male: " << male << "\n";
  Rcpp::Rcout << "known_height: " << known_height << "\n";
  Rcpp::Rcout << "height_percentile: " << height_percentile << "\n";
  Rcpp::Rcout << "source: " << source << "\n";
  Rcpp::Rcout << "type: " << type << "\n";

  if (!(male == 0 || male == 1)) {
    Rf_error("male needs to be a 0 or 1");
  }

  if (source == "gemelli1990") {
    if (male == 1) {
      LUT = gemelli1990_male();
    } else {
      LUT = gemelli1990_female();
    }
  } else if (source == "lo2013") {
    if (male == 1) {
      LUT = lo2013_male();
    } else {
      LUT = lo2013_female();
    }
  } else if (source == "nhlbi") {
    if (male == 1) {
      LUT = nhlbi_male();
    } else {
      LUT = nhlbi_female();
    }
  } else if (source == "flynn2017") {
    if (male == 1) {
      LUT = flynn2017_male();
    } else {
      LUT = flynn2017_female();
    }
  } else if (source == "martin2022") {
    if (age < 12) {
      if (male == 1) {
        LUT = gemelli1990_male();
      } else {
        LUT = gemelli1990_female();
      }
    } else if (known_height == 0) {
      if (age < 36) {
        if (male == 1) {
          LUT = nhlbi_male();
        } else {
          LUT = nhlbi_female();
        }
      } else {
        if (male == 1) {
          LUT = lo2013_male();
        } else {
          LUT = lo2013_female();
        }
      }
    } else { // height is known
      if (male == 1) {
        LUT = nhlbi_male();
      } else {
        LUT = nhlbi_female();
      }
    }
  } else {
    Rf_error("Unknown source");
  }

  arma::uvec aindex = arma::find((LUT.col(0) <= age) && (abs(LUT.col(5) - height_percentile*100)) == arma::min(abs(LUT.col(5) - height_percentile*100)));

  if (aindex.n_elem == 0) {
    Rcpp::NumericVector rtn (9);
    for (int i = 0; i < 9; ++i) {
      rtn(i) = NA_REAL;
    }
    return rtn;
  }  else {
    LUT = LUT.row(aindex(aindex.n_elem - 1));
    LUT.resize(LUT.n_rows, LUT.n_cols + 2);

    if (type == "percentile") {
      LUT.col(LUT.n_cols - 2) = R::pnorm(sbp, LUT.col(1)(0), LUT.col(2)(0), 1, 0);
      LUT.col(LUT.n_cols - 1) = R::pnorm(dbp, LUT.col(3)(0), LUT.col(4)(0), 1, 0);
    } else {
      LUT.col(LUT.n_cols - 2) = R::qnorm(sbp, LUT.col(1)(0), LUT.col(2)(0), 1, 0);
      LUT.col(LUT.n_cols - 1) = R::qnorm(dbp, LUT.col(3)(0), LUT.col(4)(0), 1, 0);
    }

    return Rcpp::wrap(LUT);
  } 

}

// -------------------------------------------------------------------------- //
//                             Exported Functions                             //
// -------------------------------------------------------------------------- //

//' @title Pediatric Growth Standards
//'
//' @description Pediatric growth standard based on LMS data from the CDC and WHO.
//'
//' @details expect to call this from R after checking some functional
//' arguments within R.
//'
//' @param metric string
//' @param source string
//' @param male  integer
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
  //
  // vector length - either length 1, or equal length.
  int max_length = std::max({metric.length(), source.length(), male.length(), x.length(), qp.length(), type.length()});
  int min_length = std::min({metric.length(), source.length(), male.length(), x.length(), qp.length(), type.length()});

  if (min_length == 0) {
    Rf_error("zero length vector");
  }

  if (max_length > 1) {

    if (metric.length() == 1) {
      metric = resize(metric, max_length);
      metric.fill(metric(0));
    } else if (metric.length() > 1 && metric.length() < max_length) {
      Rf_error("all input vectors need to be of equal length, or length 1.");
    } else {
      // metric should be the same length as max_length and there is nothing
      // to do
    }

    if (source.length() == 1) {
      source = resize(source, max_length);
      source.fill(source(0));
    } else if (source.length() > 1 && source.length() < max_length) {
      Rf_error("all input vectors need to be of equal length, or length 1.");
    } else {
      // source should be the same length as max_length and there is nothing
      // to do
    }

    if (male.length() == 1) {
      male = resize(male, max_length);
      male.fill(male(0));
    } else if (male.length() > 1 && male.length() < max_length) {
      Rf_error("all input vectors need to be of equal length, or length 1.");
    } else {
      // male should be the same length as max_length and there is nothing
      // to do
    }

    if (x.length() == 1) {
      x = resize(x, max_length);
      x.fill(x(0));
    } else if (x.length() > 1 && x.length() < max_length) {
      Rf_error("all input vectors need to be of equal length, or length 1.");
    } else {
      // x should be the same length as max_length and there is nothing
      // to do
    }

    if (qp.length() == 1) {
      qp = resize(qp, max_length);
      qp.fill(qp(0));
    } else if (qp.length() > 1 && qp.length() < max_length) {
      Rf_error("all input vectors need to be of equal length, or length 1.");
    } else {
      // qp should be the same length as max_length and there is nothing
      // to do
    }

    if (type.length() == 1) {
      type = resize(type, max_length);
      type.fill(type(0));
    } else if (type.length() > 1 && type.length() < max_length) {
      Rf_error("all input vectors need to be of equal length, or length 1.");
    } else {
      // type should be the same length as max_length and there is nothing
      // to do
    }

  } else {
    // max_length is 1 since test for zero is above an there is nothing to do
  }

  Rcpp::NumericVector rtn (max_length);
  for(int i = 0; i < max_length; ++i) {
    rtn(i) = cppPGSF1(Rcpp::as<std::string>(metric(i)), Rcpp::as<std::string>(source(i)), male(i), x(i), qp(i), Rcpp::as<std::string>(type(i)));
  }
  return rtn;
}



//' @title Pediatric Blood Pressure
//'
//' @description Pediatric Blood Pressure quantiles and percentiles
//'
//' @detail
//'
//' @param qp_sbp the quantile(s) or percentile(s) for systolic blood pressure
//' @param qp_dbp the quantile(s) or percentile(s) for diastolic blood pressure
//' @param age numeric vector, in months
//' @param male integer vector; 0 = female, 1 = male
//' @param height numeric vector of stature
//' @param default_height_percentile default height percentile to use if \code{height} is missing
//' @param source the method, or data set, to use as the reference.
//' @param type quantile or percentile to return
//
// [[Rcpp::export]]
Rcpp::List cppBP(
    Rcpp::NumericVector qp_sbp,
    Rcpp::NumericVector qp_dbp,
    Rcpp::NumericVector age,
    Rcpp::IntegerVector male,
    Rcpp::NumericVector height,
    double default_height_percentile,
    Rcpp::CharacterVector source,
    Rcpp::CharacterVector type
    )
{

  // input length of qp_sbp and qp_dbp need to be the same
  if (qp_sbp.length() != qp_dbp.length()) {
    Rf_error("qp_sbp and qp_dbp lengths are not equal");
  }

  if (source.length() != 1) {
    Rf_error("'source' should have length 1");
  }
  if (type.length() != 1) {
    Rf_error("'type' should have length 1");
  }
  // vector length - either length 1, or equal length.
  int max_length = std::max({qp_sbp.length(), qp_dbp.length(), age.length(), male.length(), height.length()});
  int min_length = std::min({qp_sbp.length(), qp_dbp.length(), age.length(), male.length(), height.length()});

  if (min_length == 0) {
    Rf_error("zero length vector");
  }

  if (max_length > 1) {
    if (qp_sbp.length() == 1) {
      qp_sbp = resize(qp_sbp, max_length);
      qp_sbp.fill(qp_sbp(0));
    } else if (qp_sbp.length() > 1 && qp_sbp.length() < max_length) {
      Rf_error("all input vectors need to be of equal length, or length 1.");
    } else {
      // nothing to do
    }
    if (qp_dbp.length() == 1) {
      qp_dbp = resize(qp_dbp, max_length);
      qp_dbp.fill(qp_dbp(0));
    } else if (qp_dbp.length() > 1 && qp_dbp.length() < max_length) {
      Rf_error("all input vectors need to be of equal length, or length 1.");
    } else {
      // nothing to do
    }
    if (age.length() == 1) {
      age = resize(age, max_length);
      age.fill(age(0));
    } else if (age.length() > 1 && age.length() < max_length) {
      Rf_error("all input vectors need to be of equal length, or length 1.");
    } else {
      // nothing to do
    }
    if (male.length() == 1) {
      male = resize(male, max_length);
      male.fill(male(0));
    } else if (male.length() > 1 && male.length() < max_length) {
      Rf_error("all input vectors need to be of equal length, or length 1.");
    } else {
      // nothing to do
    }
    if (height.length() == 1) {
      height = resize(height, max_length);
      height.fill(height(0));
    } else if (height.length() > 1 && height.length() < max_length) {
      Rf_error("all input vectors need to be of equal length, or length 1.");
    } else {
      // nothing to do
    }
    source = resize(source, max_length);
    source.fill(source(0));
    type = resize(type, max_length);
    type.fill(type(0));
  }

  Rcpp::NumericVector height_percentile (max_length);
  Rcpp::LogicalVector known_height = Rcpp::is_na(height);
  Rcpp::NumericMatrix lutbp (max_length, 9);
  int i = 0;

  for (i = 0; i < max_length; ++i) {
    if (known_height(i)) {
      if (age(i) < 36) {
        height_percentile(i) = cppPGSF1("length_for_age", "WHO", male(i), age(i), height(i), "percentile");
      } else {
        height_percentile(i) = cppPGSF1("height_for_age", "CDC", male(i), age(i), height(i), "percentile");
      }
    } else {
      height_percentile(i) = default_height_percentile;
    }
  }

  for (i = 0; i < max_length; ++i) {
    lutbp(i, _) = cppBPF1(
        qp_sbp(i),
        qp_dbp(i),
        age(i),
        male(i),
        known_height(i),
        height_percentile(i),
        Rcpp::as<std::string>(source(i)),
        Rcpp::as<std::string>(type(i))
        );
  }

  //return lutbp;

  // Create Return object
  Rcpp::List rtn;
  if (type(0) == "percentile") {
    rtn = Rcpp::List::create(_["sbp_percentile"] = lutbp(_, 7), _["dbp_percentile"] = lutbp(_, 8));
  } else {
    rtn = Rcpp::List::create(_["sbp"] = lutbp(_, 7), _["dbp"] = lutbp(_, 8));
  }

  Rcpp:: CharacterVector src(max_length);
  for(i = 0; i < max_length; ++i) {
    if (lutbp(i, 6) == 1) {
      src(i) = "gemelli1990";
    } else if (lutbp(i, 6) == 2) {
      src(i) = "lo2013";
    } else if (lutbp(i, 6) == 3) {
      src(i) = "nhlbi";
    } else if (lutbp(i, 6) == 4) {
      src(i) = "flynn2017";
    } else if (NumericVector::is_na(lutbp(i, 6))) {
      src(i) = NA_STRING;
    } else {
      Rcpp::Rcout << lutbp(i, 6) << "\n";
      Rf_error("unknown source");
    }
    if (lutbp(i, 5) == 101) {
      height_percentile(i) = NA_REAL;
    } else {
      height_percentile(i) = lutbp(i, 5);
    }
  }

  Rcpp::DataFrame df = Rcpp::DataFrame::create(
      _["source"]   = src,
      _["male"]     = male,
      _["age"]      = lutbp(_, 0),
      _["sbp_mean"] = lutbp(_, 1),
      _["sbp_sd"]   = lutbp(_, 2),
      _["dbp_mean"] = lutbp(_, 3),
      _["dbp_sd"]   = lutbp(_, 4),
      _["height_percentile"] = height_percentile
      );

  rtn.attr("bp_params") = df;

  return rtn;

}


//
// -------------------------------------------------------------------------- //
//                                End of File                                 //
// -------------------------------------------------------------------------- //
