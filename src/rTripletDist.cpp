#include "TripletDistanceCalculator.h"
#include "int_stuff.h"

#include <R.h>
#include <Rdefines.h>
#include <Rinternals.h>
#include <cstdlib>
#include <vector>

extern "C" {

  #ifdef _WIN32
	__declspec(dllexport)
  #endif
  SEXP cTripletDistance(SEXP filename1_sexp, SEXP filename2_sexp) {
    const char *filename1;
    const char *filename2;

    if(!isString(filename1_sexp) || length(filename1_sexp) < 1
       || !isString(filename2_sexp) || length(filename2_sexp) < 1) {
      error("The two parameters to tripletDistance(filename1, filename2) should be strings.");
    }

    filename1 = CHAR(STRING_ELT(filename1_sexp,0));
    filename2 = CHAR(STRING_ELT(filename2_sexp,0));

    TripletDistanceCalculator tripletCalc;

    INTTYPE_REST res = tripletCalc.calculateTripletDistance(filename1, filename2);
    SEXP res_sexp;
    PROTECT(res_sexp = NEW_INTEGER(1));
    INTEGER_POINTER(res_sexp)[0] = res;

    UNPROTECT(1);
    return res_sexp;
  }

  #ifdef _WIN32
	__declspec(dllexport)
  #endif
  SEXP cPairsTripletDistance(SEXP filename1_sexp, SEXP filename2_sexp) {
    const char * filename1;
    if(!isString(filename1_sexp) || length(filename1_sexp) < 1) {
      error("The parameter to pairsTripletDistance(filename) should be strings.");
    }
    filename1 = CHAR(STRING_ELT(filename1_sexp,0));
    const char * filename2;
    if(!isString(filename2_sexp) || length(filename2_sexp) < 1) {
      error("The parameter to pairsTripletDistance(filename) should be strings.");
    }
    filename2 = CHAR(STRING_ELT(filename2_sexp,0));

    TripletDistanceCalculator tripletCalc;
    std::vector<INTTYPE_REST> res = tripletCalc.pairs_triplet_distance(filename1, filename2);
    
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
  SEXP cAllPairsTripletDistance(SEXP filename_sexp) {
    const char * filename;
    if(!isString(filename_sexp) || length(filename_sexp) < 1) {
      error("The parameter to allPairsTripletDistance(filename) should be a string.");
    }
    filename = CHAR(STRING_ELT(filename_sexp,0));

    TripletDistanceCalculator tripletCalc;
    std::vector<std::vector<INTTYPE_REST> > res = tripletCalc.calculateAllPairsTripletDistance(filename);

    SEXP res_sexp;
    PROTECT(res_sexp = allocMatrix(INTSXP, res.size(), res.size()));
    int *ians = INTEGER(res_sexp);
    for(size_t r = 0; r < res.size(); ++r) {
      for(size_t c = 0; c < r; ++c) {
	ians[r + res.size()*c] = res[r][c];
	ians[c + res.size()*r] = res[r][c];
      }
      ians[r + res.size()*r] = res[r][r];
    }

    UNPROTECT(1);
    return res_sexp;
  }

}
