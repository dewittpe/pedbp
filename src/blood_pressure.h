#include <Rcpp.h>
#include <Rmath.h>
#ifndef pedbp_blood_pressure_H
#define pedbp_blood_pressure_H

// cppBPF1 - Blood Pressure Function 1
//
// Figure out and retrieve the needed look up table and return the needed
// parameters, quantiles or percentiles.
//
// detailed documentation is in the blood_pressure.cpp file
//
Rcpp::NumericVector cppBPF1(
    double sbp,
    double dbp,
    double age,
    int male,
    int known_height,
    double height_percentile,
    std::string source,
    std::string type);

// cppBP - Blood Pressures
//
// Exported to R
//
// detailed documentation is in the blood_pressure.cpp file
//
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
    );

#endif

// -------------------------------------------------------------------------- //
//                                End of File                                 //
// -------------------------------------------------------------------------- //
