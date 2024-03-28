#include <Rcpp.h>
#include <Rmath.h>
#ifndef pedbp_utilities_H
#define pedbp_utilities_H
Rcpp::NumericVector resize(Rcpp::NumericVector x, int length);
Rcpp::CharacterVector resize(Rcpp::CharacterVector x, int length);
Rcpp::IntegerVector resize(Rcpp::IntegerVector x, int length);
#endif
