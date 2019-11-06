// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// tqdist_QuartetDistance
IntegerVector tqdist_QuartetDistance(CharacterVector file1, CharacterVector file2);
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
// tqdist_QuartetAgreement
IntegerVector tqdist_QuartetAgreement(CharacterVector file1, CharacterVector file2);
RcppExport SEXP _Quartet_tqdist_QuartetAgreement(SEXP file1SEXP, SEXP file2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type file1(file1SEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type file2(file2SEXP);
    rcpp_result_gen = Rcpp::wrap(tqdist_QuartetAgreement(file1, file2));
    return rcpp_result_gen;
END_RCPP
}
// tqdist_PairsQuartetDistance
IntegerVector tqdist_PairsQuartetDistance(CharacterVector file1, CharacterVector file2);
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
// tqdist_OneToManyQuartetAgreement
IntegerVector tqdist_OneToManyQuartetAgreement(CharacterVector file1, CharacterVector fileMany);
RcppExport SEXP _Quartet_tqdist_OneToManyQuartetAgreement(SEXP file1SEXP, SEXP fileManySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type file1(file1SEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type fileMany(fileManySEXP);
    rcpp_result_gen = Rcpp::wrap(tqdist_OneToManyQuartetAgreement(file1, fileMany));
    return rcpp_result_gen;
END_RCPP
}
// tqdist_OneToManyQuartetAgreementChar
IntegerVector tqdist_OneToManyQuartetAgreementChar(CharacterVector tree, CharacterVector trees);
RcppExport SEXP _Quartet_tqdist_OneToManyQuartetAgreementChar(SEXP treeSEXP, SEXP treesSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type tree(treeSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type trees(treesSEXP);
    rcpp_result_gen = Rcpp::wrap(tqdist_OneToManyQuartetAgreementChar(tree, trees));
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
// tqdist_AllPairsQuartetDistanceChar
IntegerMatrix tqdist_AllPairsQuartetDistanceChar(CharacterVector string);
RcppExport SEXP _Quartet_tqdist_AllPairsQuartetDistanceChar(SEXP stringSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type string(stringSEXP);
    rcpp_result_gen = Rcpp::wrap(tqdist_AllPairsQuartetDistanceChar(string));
    return rcpp_result_gen;
END_RCPP
}
// tqdist_AllPairsQuartetAgreementChar
IntegerMatrix tqdist_AllPairsQuartetAgreementChar(CharacterVector string);
RcppExport SEXP _Quartet_tqdist_AllPairsQuartetAgreementChar(SEXP stringSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type string(stringSEXP);
    rcpp_result_gen = Rcpp::wrap(tqdist_AllPairsQuartetAgreementChar(string));
    return rcpp_result_gen;
END_RCPP
}
// tqdist_AllPairsQuartetAgreement
IntegerMatrix tqdist_AllPairsQuartetAgreement(CharacterVector file);
RcppExport SEXP _Quartet_tqdist_AllPairsQuartetAgreement(SEXP fileSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type file(fileSEXP);
    rcpp_result_gen = Rcpp::wrap(tqdist_AllPairsQuartetAgreement(file));
    return rcpp_result_gen;
END_RCPP
}
// tqdist_TripletDistance
IntegerVector tqdist_TripletDistance(CharacterVector file1, CharacterVector file2);
RcppExport SEXP _Quartet_tqdist_TripletDistance(SEXP file1SEXP, SEXP file2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type file1(file1SEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type file2(file2SEXP);
    rcpp_result_gen = Rcpp::wrap(tqdist_TripletDistance(file1, file2));
    return rcpp_result_gen;
END_RCPP
}
// tqdist_PairsTripletDistance
IntegerVector tqdist_PairsTripletDistance(CharacterVector file1, CharacterVector file2);
RcppExport SEXP _Quartet_tqdist_PairsTripletDistance(SEXP file1SEXP, SEXP file2SEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type file1(file1SEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type file2(file2SEXP);
    rcpp_result_gen = Rcpp::wrap(tqdist_PairsTripletDistance(file1, file2));
    return rcpp_result_gen;
END_RCPP
}
// tqdist_AllPairsTripletDistance
IntegerMatrix tqdist_AllPairsTripletDistance(CharacterVector file);
RcppExport SEXP _Quartet_tqdist_AllPairsTripletDistance(SEXP fileSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< CharacterVector >::type file(fileSEXP);
    rcpp_result_gen = Rcpp::wrap(tqdist_AllPairsTripletDistance(file));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_Quartet_tqdist_QuartetDistance", (DL_FUNC) &_Quartet_tqdist_QuartetDistance, 2},
    {"_Quartet_tqdist_QuartetAgreement", (DL_FUNC) &_Quartet_tqdist_QuartetAgreement, 2},
    {"_Quartet_tqdist_PairsQuartetDistance", (DL_FUNC) &_Quartet_tqdist_PairsQuartetDistance, 2},
    {"_Quartet_tqdist_OneToManyQuartetAgreement", (DL_FUNC) &_Quartet_tqdist_OneToManyQuartetAgreement, 2},
    {"_Quartet_tqdist_OneToManyQuartetAgreementChar", (DL_FUNC) &_Quartet_tqdist_OneToManyQuartetAgreementChar, 2},
    {"_Quartet_tqdist_AllPairsQuartetDistance", (DL_FUNC) &_Quartet_tqdist_AllPairsQuartetDistance, 1},
    {"_Quartet_tqdist_AllPairsQuartetDistanceChar", (DL_FUNC) &_Quartet_tqdist_AllPairsQuartetDistanceChar, 1},
    {"_Quartet_tqdist_AllPairsQuartetAgreementChar", (DL_FUNC) &_Quartet_tqdist_AllPairsQuartetAgreementChar, 1},
    {"_Quartet_tqdist_AllPairsQuartetAgreement", (DL_FUNC) &_Quartet_tqdist_AllPairsQuartetAgreement, 1},
    {"_Quartet_tqdist_TripletDistance", (DL_FUNC) &_Quartet_tqdist_TripletDistance, 2},
    {"_Quartet_tqdist_PairsTripletDistance", (DL_FUNC) &_Quartet_tqdist_PairsTripletDistance, 2},
    {"_Quartet_tqdist_AllPairsTripletDistance", (DL_FUNC) &_Quartet_tqdist_AllPairsTripletDistance, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_Quartet(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
