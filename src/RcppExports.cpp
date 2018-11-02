// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// phangorn_bipCPP
List phangorn_bipCPP(IntegerMatrix orig, int nTips);
RcppExport SEXP _Quartet_phangorn_bipCPP(SEXP origSEXP, SEXP nTipsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type orig(origSEXP);
    Rcpp::traits::input_parameter< int >::type nTips(nTipsSEXP);
    rcpp_result_gen = Rcpp::wrap(phangorn_bipCPP(orig, nTips));
    return rcpp_result_gen;
END_RCPP
}
// tqdist_QuartetDistance
NumericVector tqdist_QuartetDistance(CharacterVector file1, CharacterVector file2);
RcppExport SEXP _Quartet_tqdist_QuartetDistance(SEXP file1SEXP, SEXP file2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type file1(file1SEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type file2(file2SEXP);
    rcpp_result_gen = Rcpp::wrap(tqdist_QuartetDistance(file1, file2));
    return rcpp_result_gen;
END_RCPP
}
// tqdist_PairsQuartetDistance
NumericVector tqdist_PairsQuartetDistance(CharacterVector file1, CharacterVector file2);
RcppExport SEXP _Quartet_tqdist_PairsQuartetDistance(SEXP file1SEXP, SEXP file2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type file1(file1SEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type file2(file2SEXP);
    rcpp_result_gen = Rcpp::wrap(tqdist_PairsQuartetDistance(file1, file2));
    return rcpp_result_gen;
END_RCPP
}
// tqdist_AllPairsQuartetDistance
IntegerMatrix tqdist_AllPairsQuartetDistance(CharacterVector file);
RcppExport SEXP _Quartet_tqdist_AllPairsQuartetDistance(SEXP fileSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type file(fileSEXP);
    rcpp_result_gen = Rcpp::wrap(tqdist_AllPairsQuartetDistance(file));
    return rcpp_result_gen;
END_RCPP
}
// tqdist_TripletDistance
NumericVector tqdist_TripletDistance(SEXP file1, SEXP file2);
RcppExport SEXP _Quartet_tqdist_TripletDistance(SEXP file1SEXP, SEXP file2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type file1(file1SEXP);
    Rcpp::traits::input_parameter< SEXP >::type file2(file2SEXP);
    rcpp_result_gen = Rcpp::wrap(tqdist_TripletDistance(file1, file2));
    return rcpp_result_gen;
END_RCPP
}
// tqdist_PairsTripletDistance
NumericVector tqdist_PairsTripletDistance(SEXP file1, SEXP file2);
RcppExport SEXP _Quartet_tqdist_PairsTripletDistance(SEXP file1SEXP, SEXP file2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type file1(file1SEXP);
    Rcpp::traits::input_parameter< SEXP >::type file2(file2SEXP);
    rcpp_result_gen = Rcpp::wrap(tqdist_PairsTripletDistance(file1, file2));
    return rcpp_result_gen;
END_RCPP
}
// tqdist_AllPairsTripletDistance
IntegerMatrix tqdist_AllPairsTripletDistance(SEXP file);
RcppExport SEXP _Quartet_tqdist_AllPairsTripletDistance(SEXP fileSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< SEXP >::type file(fileSEXP);
    rcpp_result_gen = Rcpp::wrap(tqdist_AllPairsTripletDistance(file));
    return rcpp_result_gen;
END_RCPP
}
