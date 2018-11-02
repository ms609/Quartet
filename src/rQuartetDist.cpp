#include <Rcpp.h>
using namespace Rcpp;
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

//' Triplet and quartet distances with tqDist
//' 
//' Functions to calculate triplet and quartet distances between pairs of trees.
//' Input is not checked for sanity.
//' 
//' @param file,file1,file2 Paths to files containing a tree or trees in Newick format.
//' 
//' @return The distance between the requested trees.
//' 
//' @author Martin R. Smith, after Andreas Sand
//' 
//' @references \insertRef{Sand2014}{Quartet}
//' 
//' @keywords internal
//' @export
// [[Rcpp::export]]
NumericVector tqdist_QuartetDistance(CharacterVector file1, CharacterVector file2) {
  int n1 = file1.size(), n2 = file2.size();
  if (n1 != 1 || n2 != 1) {
    Rcpp::stop("file1 and file2 must be character vectors of length 1");
  }
  
  const char *filename1;
  const char *filename2;
  
  filename1 = CHAR(STRING_ELT(file1, 0));
  filename2 = CHAR(STRING_ELT(file2, 0));
  
  QuartetDistanceCalculator quartetCalc;
  
  INTTYPE_N4 res = quartetCalc.calculateQuartetDistance(filename1, filename2);
  NumericVector NV_res(1);
  NV_res = res;
  return NV_res;
}

//' @describeIn tqdist_QuartetDistance Distance between pairs
//' @export
// [[Rcpp::export]]
NumericVector tqdist_PairsQuartetDistance(CharacterVector file1, CharacterVector file2) {
  int n1 = file1.size(), n2 = file2.size();
  if (n1 != 1 || n2 != 1) {
    Rcpp::stop("file1 and file2 must be character vectors of length 1");
  }
  
  const char *filename1;
  const char *filename2;
  
  filename1 = CHAR(STRING_ELT(file1, 0));
  filename2 = CHAR(STRING_ELT(file2, 0));
  
  QuartetDistanceCalculator quartetCalc;
  
  std::vector<INTTYPE_N4> res = quartetCalc.pairs_quartet_distance(filename1, filename2);
  
  NumericVector NV_res(res.size());
//  for (size_t i = 0; i < res.size(); ++i) {
//    NV_res[i] = res[i];
//  }
  return NV_res;
}

//' @describeIn tqdist_QuartetDistance Distance between all pairs
//' @export
// [[Rcpp::export]]
IntegerMatrix tqdist_AllPairsQuartetDistance(CharacterVector file) {
  int n = file.size();
  if (n != 1) {
    Rcpp::stop("file must be a character vector of length 1");
  }
  
  const char *filename;
  filename = CHAR(STRING_ELT(file, 0));
  QuartetDistanceCalculator quartetCalc;
  
  std::vector<std::vector<INTTYPE_N4> > res = quartetCalc.calculateAllPairsQuartetDistance(filename);
  
  IntegerMatrix IM_res(res.size(), res.size());
//  int *ians = INTEGER(res_sexp);
  
  for (size_t r = 0; r < res.size(); ++r) {
    for (size_t c = 0; c < r; ++c) {
      int current_res = int(res[r][c]);
      IM_res[r + res.size() * c] = current_res;
      IM_res[c + res.size() * r] = current_res;
    }
    IM_res[r + res.size()*r] = res[r][r];
  }
  
  return IM_res;
  