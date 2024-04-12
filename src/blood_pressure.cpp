// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <Rcpp.h>
#include <Rmath.h>
#include "growth_standards.h"
#include "bp_params.h"
#include "utilities.h"
using namespace Rcpp;

// -------------------------------------------------------------------------- //
// cpp Blood Pressure Function 1
//
// Find the probability or quantile for one observation of age, male, height
//
// args:
//    sbp: the quantile or probability (distribution funtion value) systolic blood pressure
//    dbp: the quantile or probability (distribution funtion value) diastolic blood pressure
//    age: in months
//    male: 0 = female, 1 = male
//    known_height: 0 = height is not known; 1 = height is known
//    height_percentile: the height percentile to use in the look up table.
//                       This is only relevant to nhlbi and flynn2017 data.
//                       Expected input range [0, 100]
//    source: the data source or method defining the look up table
//    type: distribution or quantile
//
//  return:
//
//    A numeric vector of lenght 9
//
//      (0) look up table age
//      (1) look up table systolic blood pressure mean
//      (2) look up table systolic blood pressure sd
//      (3) look up table diastolic blood pressure mean
//      (4) look up table diastolic blood pressure sd
//      (5) look up table height percentile
//      (6) integer value denoting source:
//          1 = gemelli1990
//          2 = lo2013
//          3 = nhlbi
//          4 = flynn2017
//      (7) systolic blood pressure - quantile or probability (distribution
//          funtion value); as defined by type
//      (8) diastolic blood pressure - quantile or probability (distribution
//          funtion value); as defined by type
//
Rcpp::NumericVector cppBPF1(double sbp, double dbp, double age, int male, int
    known_height, double height_percentile, std::string source, std::string
    type) {
  arma::mat LUT;

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

  arma::uvec aindex = arma::find((LUT.col(0) <= age) && (abs(LUT.col(5) - height_percentile)) == arma::min(abs(LUT.col(5) - height_percentile)));

  if (aindex.n_elem == 0 || age > 216.0 || (source == "gemelli1990" && age > 12)) {
    Rcpp::NumericVector rtn (9);
    for (int i = 0; i < 9; ++i) {
      rtn(i) = NA_REAL;
    }
    return rtn;
  }  else {
    LUT = LUT.row(aindex(aindex.n_elem - 1));
    LUT.resize(LUT.n_rows, LUT.n_cols + 2);

    if (type == "distribution") {
      LUT.col(LUT.n_cols - 2) = R::pnorm(sbp, LUT.col(1)(0), LUT.col(2)(0), 1, 0);
      LUT.col(LUT.n_cols - 1) = R::pnorm(dbp, LUT.col(3)(0), LUT.col(4)(0), 1, 0);
    } else if (type == "quantile") {
      LUT.col(LUT.n_cols - 2) = R::qnorm(sbp, LUT.col(1)(0), LUT.col(2)(0), 1, 0);
      LUT.col(LUT.n_cols - 1) = R::qnorm(dbp, LUT.col(3)(0), LUT.col(4)(0), 1, 0);
    } else if (type == "zscore") {
      LUT.col(LUT.n_cols - 2) = (sbp - LUT.col(1)(0)) / LUT.col(2)(0);
      LUT.col(LUT.n_cols - 1) = (dbp - LUT.col(3)(0)) / LUT.col(4)(0);
    } else {
      Rf_error("type needs to be one of 'distribution', 'quantile', or 'zscore'");
    }

    return Rcpp::wrap(LUT);
  }
}

// -------------------------------------------------------------------------- //

//' @title Pediatric Blood Pressure
//'
//' @description Pediatric Blood Pressure quantile and probability
//' (distribution function) values
//'
//' @details
//'
//' \code{height} is used preferentially over \code{height_percentile} over
//' \code{default_height_percentile}.
//'
//' \code{source} can be one of \code{"gemelli1990"}, \code{"lo2013"},
//' \code{"nhlbi"}, \code{"flynn2017"}, or \code{"martin2022"}.
//'
//' @param qp_sbp the quantile(s) or probability(s) for systolic blood pressure
//' @param qp_dbp the quantile(s) or probability(s) for diastolic blood pressure
//' @param age numeric vector, in months
//' @param male integer vector; 0 = female, 1 = male
//' @param height numeric vector of stature
//' @param height_percentile numeric vector for height percentiles, expected
//'        values between 0 and 1.
//' @param default_height_percentile default height percentile to use if \code{height} is missing
//' @param source the method, or data set, to use as the reference.
//' @param type quantile or distribution to return
//'
//' @return
//' A list:
//'
//' [[1]] systolic blood pressure quantiles or probability (defined by the input value of \code{type}).
//' [[2]] diastolic blood pressure quantiles or probability (defined by the input value of \code{type}).
//'
//' \code{attr(, "bp_params")} is a \code{data.frame} with the values for the
//' look up table(s) needed to inform the sbp and dbp values.
// [[Rcpp::export]]
Rcpp::List cppBP(
    Rcpp::NumericVector qp_sbp,
    Rcpp::NumericVector qp_dbp,
    Rcpp::NumericVector age,
    Rcpp::IntegerVector male,
    Rcpp::NumericVector height,
    Rcpp::NumericVector height_percentile,
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
  int max_length = std::max({qp_sbp.length(), qp_dbp.length(), age.length(), male.length(), height.length(), height_percentile.length()});
  int min_length = std::min({qp_sbp.length(), qp_dbp.length(), age.length(), male.length(), height.length(), height_percentile.length()});

  if (min_length == 0) {
    Rf_error("zero length vector");
  }

  if (max_length > 1) {
    if ( (qp_sbp.length()            > 1 && qp_sbp.length()            < max_length) ||
         (qp_dbp.length()            > 1 && qp_dbp.length()            < max_length) ||
         (age.length()               > 1 && age.length()               < max_length) ||
         (male.length()              > 1 && male.length()              < max_length) ||
         (height.length()            > 1 && height.length()            < max_length) ||
         (height_percentile.length() > 1 && height_percentile.length() < max_length)) {
      Rf_error("all input vectors need to be of equal length, or length 1.");
    }

    if (qp_sbp.length() == 1) {
      qp_sbp = resize(qp_sbp, max_length);
      qp_sbp.fill(qp_sbp(0));
    }

    if (qp_dbp.length() == 1) {
      qp_dbp = resize(qp_dbp, max_length);
      qp_dbp.fill(qp_dbp(0));
    }

    if (age.length() == 1) {
      age = resize(age, max_length);
      age.fill(age(0));
    }

    if (male.length() == 1) {
      male = resize(male, max_length);
      male.fill(male(0));
    }

    if (height.length() == 1) {
      height = resize(height, max_length);
      height.fill(height(0));
    }

    if (height_percentile.length() == 1) {
      height_percentile = resize(height_percentile, max_length);
      height_percentile.fill(height_percentile(0));
    }

    source = resize(source, max_length);
    source.fill(source(0));
    type = resize(type, max_length);
    type.fill(type(0));
  }

  Rcpp::LogicalVector known_height = !Rcpp::is_na(height);
  Rcpp::LogicalVector known_heightp = !Rcpp::is_na(height_percentile);
  Rcpp::NumericVector hp(max_length);
  Rcpp::NumericMatrix lutbp (max_length, 9);
  int i = 0;

  for (i = 0; i < max_length; ++i) {
    if (known_height(i)) {
      if (age(i) < 36) {
        hp(i) = cppPGSF1("length_for_age", "WHO", male(i), age(i), height(i), "distribution");
      } else {
        hp(i) = cppPGSF1("height_for_age", "CDC", male(i), age(i), height(i), "distribution");
      }
    } else {
      if (known_heightp(i)) {
        known_height(i) = 1;
        hp(i) = height_percentile(i);
      } else {
        hp(i) = default_height_percentile;
      }
    }
  }

  for (i = 0; i < max_length; ++i) {
    lutbp(i, _) = cppBPF1(qp_sbp(i), qp_dbp(i), age(i), male(i),
                          known_height(i), hp(i),
                          Rcpp::as<std::string>(source(i)),
                          Rcpp::as<std::string>(type(i))
                          );
  }

  // Create Return object
  Rcpp::List rtn;
  if (type(0) == "distribution") {
    rtn = Rcpp::List::create(_["sbp_p"] = lutbp(_, 7), _["dbp_p"] = lutbp(_, 8));
  } else if (type(0) == "quantile") {
    rtn = Rcpp::List::create(_["sbp"] = lutbp(_, 7), _["dbp"] = lutbp(_, 8));
  } else if (type(0) == "zscore") {
    rtn = Rcpp::List::create(_["sbp_z"] = lutbp(_, 7), _["dbp_z"] = lutbp(_, 8));
  } else {
    // this is here to be robust but should be impossible to get to as the call
    // to cppBPF1 will error out first
    Rf_error("type needs to be one of 'percentile', 'quantile', or 'zscore'");
  }

  Rcpp::CharacterVector src(max_length);

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
      // this is here to be robust but should be impossible to get to as the
      // call to cppBPF1 will error out first
      Rf_error("unknown source");
    }
    if (lutbp(i, 5) == 101) {
      hp(i) = NA_REAL;
    } else {
      hp(i) = lutbp(i, 5);
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
      _["height_percentile"] = hp
      );

  rtn.attr("bp_params") = df;

  return rtn;

}


//
// -------------------------------------------------------------------------- //
//                                End of File                                 //
// -------------------------------------------------------------------------- //
