#include <Rcpp.h>
#include <Rmath.h>
#include "utilities.h"

// Resize
//
// Resize a R-like vector while retaining data
//
// Args:
//   x  the vector to resize
//   lenght the size of the resulting vector
//
// Return:
//    a vector of the same type as x
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
//                                End of File                                 //
// -------------------------------------------------------------------------- //
