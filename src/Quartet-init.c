#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern SEXP _Quartet_phangorn_bipCPP(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_Quartet_phangorn_bipCPP", (DL_FUNC) &_Quartet_phangorn_bipCPP, 2},
    {NULL, NULL, 0}
};

void R_init_Quartet(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
