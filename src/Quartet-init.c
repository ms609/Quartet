#include <R.h>
#include <Rinternals.h> /* for SEXP */
#include <stdlib.h> /* for NULL */
#include <R_ext/Rdynload.h>

extern SEXP _Quartet_phangorn_bipCPP(SEXP, SEXP);

extern SEXP _Quartet_tqdist_QuartetDistance(SEXP, SEXP);
extern SEXP _Quartet_tqdist_QuartetAgreement(SEXP, SEXP);
extern SEXP _Quartet_tqdist_OneToManyQuartetAgreement(SEXP, SEXP);
extern SEXP _Quartet_tqdist_PairsQuartetDistance(SEXP, SEXP);
extern SEXP _Quartet_tqdist_AllPairsQuartetDistance(SEXP);
extern SEXP _Quartet_tqdist_AllPairsQuartetAgreement(SEXP);

extern SEXP _Quartet_tqdist_TripletDistance(SEXP, SEXP);
extern SEXP _Quartet_tqdist_PairsTripletDistance(SEXP, SEXP);
extern SEXP _Quartet_tqdist_AllPairsTripletDistance(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_Quartet_tqdist_QuartetDistance", (DL_FUNC) &_Quartet_tqdist_QuartetDistance, 2},
    {"_Quartet_tqdist_QuartetAgreement", (DL_FUNC) &_Quartet_tqdist_QuartetAgreement, 2},
    {"_Quartet_tqdist_PairsQuartetDistance", (DL_FUNC) &_Quartet_tqdist_PairsQuartetDistance, 2},
    {"_Quartet_tqdist_AllPairsQuartetDistance", (DL_FUNC) &_Quartet_tqdist_AllPairsQuartetDistance, 1},
    {"_Quartet_tqdist_OneToManyQuartetAgreement", (DL_FUNC) &_Quartet_tqdist_OneToManyQuartetAgreement, 2},
    {"_Quartet_tqdist_AllPairsQuartetAgreement", (DL_FUNC) &_Quartet_tqdist_AllPairsQuartetAgreement, 1},
    {"_Quartet_tqdist_TripletDistance", (DL_FUNC) &_Quartet_tqdist_TripletDistance, 2},
    {"_Quartet_tqdist_PairsTripletDistance", (DL_FUNC) &_Quartet_tqdist_PairsTripletDistance, 2},
    {"_Quartet_tqdist_AllPairsTripletDistance", (DL_FUNC) &_Quartet_tqdist_AllPairsTripletDistance, 1},
    {NULL, NULL, 0}
};

void R_init_Quartet(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
