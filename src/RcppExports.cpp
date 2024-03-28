// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// cppPGSF
Rcpp::NumericVector cppPGSF(Rcpp::CharacterVector metric, Rcpp::CharacterVector source, Rcpp::IntegerVector male, Rcpp::NumericVector x, Rcpp::NumericVector qp, Rcpp::CharacterVector type);
RcppExport SEXP _pedbp_cppPGSF(SEXP metricSEXP, SEXP sourceSEXP, SEXP maleSEXP, SEXP xSEXP, SEXP qpSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type metric(metricSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type source(sourceSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type male(maleSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type qp(qpSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(cppPGSF(metric, source, male, x, qp, type));
    return rcpp_result_gen;
END_RCPP
}
// cppBP
Rcpp::List cppBP(Rcpp::NumericVector qp_sbp, Rcpp::NumericVector qp_dbp, Rcpp::NumericVector age, Rcpp::IntegerVector male, Rcpp::NumericVector height, Rcpp::NumericVector height_percentile, double default_height_percentile, Rcpp::CharacterVector source, Rcpp::CharacterVector type);
RcppExport SEXP _pedbp_cppBP(SEXP qp_sbpSEXP, SEXP qp_dbpSEXP, SEXP ageSEXP, SEXP maleSEXP, SEXP heightSEXP, SEXP height_percentileSEXP, SEXP default_height_percentileSEXP, SEXP sourceSEXP, SEXP typeSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type qp_sbp(qp_sbpSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type qp_dbp(qp_dbpSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type age(ageSEXP);
    Rcpp::traits::input_parameter< Rcpp::IntegerVector >::type male(maleSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type height(heightSEXP);
    Rcpp::traits::input_parameter< Rcpp::NumericVector >::type height_percentile(height_percentileSEXP);
    Rcpp::traits::input_parameter< double >::type default_height_percentile(default_height_percentileSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type source(sourceSEXP);
    Rcpp::traits::input_parameter< Rcpp::CharacterVector >::type type(typeSEXP);
    rcpp_result_gen = Rcpp::wrap(cppBP(qp_sbp, qp_dbp, age, male, height, height_percentile, default_height_percentile, source, type));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_pedbp_cppPGSF", (DL_FUNC) &_pedbp_cppPGSF, 6},
    {"_pedbp_cppBP", (DL_FUNC) &_pedbp_cppBP, 9},
    {NULL, NULL, 0}
};

RcppExport void R_init_pedbp(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
