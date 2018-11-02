#include "int_stuff.h"

#undef INTTYPE_N4
#ifdef WIN32
  #define INTTYPE_N4 long long
#else
#define INTTYPE_N4  __int128_t
#endif

#include "QuartetDistanceCalculator.h"

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <cstdlib>
#include <vector>

extern "C" {

  #ifdef _WIN32
	__declspec(dllexport)
  #endif
  SEXP cQuartetDistance(SEXP filename1_sexp, SEXP filename2_sexp) {
    const char *filename1;
    const char *filename2;

    if(!isString(filename1_sexp) || length(filename1_sexp) < 1
       || !isString(filename2_sexp) || length(filename2_sexp) < 1) {
      error("The two parameters to quartetDistance(filename1, filename2) should be strings.");
    }

    filename1 = CHAR(STRING_ELT(filename1_sexp,0));
    filename2 = CHAR(STRING_ELT(filename2_sexp,0));

    QuartetDistanceCalculator quartetCalc;

    INTTYPE_N4 res = quartetCalc.calculateQuartetDistance(filename1, filename2);
    SEXP res_sexp;
    PROTECT(res_sexp = NEW_INTEGER(1));
    INTEGER_POINTER(res_sexp)[0] = res;

    UNPROTECT(1);
    return res_sexp;
  }

  #ifdef _WIN32
	__declspec(dllexport)
  #endif
  SEXP cPairsQuartetDistance(SEXP filename1_sexp, SEXP filename2_sexp) {
    const char * filename1;
    if(!isString(filename1_sexp) || length(filename1_sexp) < 1) {
      error("The parameters to pairsQuartetDistance(filename) should be strings.");
    }
    filename1 = CHAR(STRING_ELT(filename1_sexp,0));
    const char * filename2;
    if(!isString(filename2_sexp) || length(filename2_sexp) < 1) {
      error("The parameters to allPairsQuartetDistance(filename) should be strings.");
    }
    filename2 = CHAR(STRING_ELT(filename2_sexp,0));

    QuartetDistanceCalculator quartetCalc;
    std::vector<INTTYPE_N4> res = quartetCalc.pairs_quartet_distance(filename1, filename2);
    
    SEXP res_sexp;
    PROTECT(res_sexp = allocVector(INTSXP, res.size()));
    int *ians = INTEGER(res_sexp);

    for(size_t i = 0; i < res.size(); ++i) {
      ians[i] = res[i];
    }
   
    UNPROTECT(1);

    return res_sexp;
  }

  #ifdef _WIN32
	__declspec(dllexport)
  #endif
  SEXP cAllPairsQuartetDistance(SEXP filename_sexp) {
    const char * filename;
    if(!isString(filename_sexp) || length(filename_sexp) < 1) {
      error("The parameter to allPairsQuartetDistance(filename) should be a string.");
    }
    filename = CHAR(STRING_ELT(filename_sexp,0));

    QuartetDistanceCalculator quartetCalc;
    std::vector<std::vector<INTTYPE_N4> > res = quartetCalc.calculateAllPairsQuartetDistance(filename);
    
    SEXP res_sexp;
    PROTECT(res_sexp = allocMatrix(INTSXP, res.size(), res.size()));
    int *ians = INTEGER(res_sexp);

    for(size_t r = 0; r < res.size(); ++r) {
      for(size_t c = 0; c < r; ++c) {
    	int current_res = int(res[r][c]);
    	ians[r + res.size()*c] = current_res;
    	ians[c + res.size()*r] = current_res;
      }
      ians[r + res.size()*r] = res[r][r];
    }

    UNPROTECT(1);

    return res_sexp;
  }
}
