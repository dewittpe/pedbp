#include <Rcpp.h>
#include <Rmath.h>
#ifndef pedbp_growth_standards_H
#define pedbp_growth_standards_H

// Pediatric Growth Standards Function 1 gets the look up table and values for
// for one observation.
double cppPGSF1(std::string metric, std::string source, int male, double x, double qp, std::string type);

// Exported to R
Rcpp::NumericVector cppPGSF(
    Rcpp::CharacterVector metric,
    Rcpp::CharacterVector source,
    Rcpp::IntegerVector male,
    Rcpp::NumericVector x,
    Rcpp::NumericVector qp,
    Rcpp::CharacterVector type
    );
#endif
