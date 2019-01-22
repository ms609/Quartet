#include <Rcpp.h>
using namespace Rcpp;

#include "int_stuff.h"
#include "TripletDistanceCalculator.h"

#include <vector>

//' @describeIn tqdist_QuartetDistance Triplet distance between two trees
//' @export
// [[Rcpp::export]]
IntegerVector tqdist_TripletDistance(CharacterVector file1, CharacterVector file2) {
  int n1 = file1.size(), n2 = file2.size();
  if (n1 != 1 || n2 != 1) {
    Rcpp::stop("file1 and file2 must be character vectors of length 1");
  }
  
  const char *filename1;
  const char *filename2;

  filename1 = CHAR(STRING_ELT(file1, 0));
  filename2 = CHAR(STRING_ELT(file2, 0));

  TripletDistanceCalculator tripletCalc;

  INTTYPE_REST res = tripletCalc.calculateTripletDistance(filename1, filename2);
  
  IntegerVector IV_res(1);
  IV_res[0] = res;
  return IV_res;
}

//' @describeIn tqdist_QuartetDistance Triplet distance between pairs
//' @export
// [[Rcpp::export]]
IntegerVector tqdist_PairsTripletDistance(CharacterVector file1, CharacterVector file2) {
  int n1 = file1.size(), n2 = file2.size();
  if (n1 != 1 || n2 != 1) {
    Rcpp::stop("file1 and file2 must be character vectors of length 1");
  }
  
  const char * filename1;
  const char * filename2;
  
  filename1 = CHAR(STRING_ELT(file1, 0));
  filename2 = CHAR(STRING_ELT(file2, 0));
  
  TripletDistanceCalculator tripletCalc;
  
  std::vector<INTTYPE_REST> res = tripletCalc.pairs_triplet_distance(filename1, filename2);
  
  IntegerVector IV_res(res.size());
  for (size_t i = 0; i < res.size(); ++i) {
    IV_res[i] = res[i];
  }
  return IV_res;
}

//' @describeIn tqdist_QuartetDistance Triplet distance between all pairs
//' @export
// [[Rcpp::export]]
IntegerMatrix tqdist_AllPairsTripletDistance(CharacterVector file) {
  int n = file.size();
  if (n != 1) {
    Rcpp::stop("file must be a character vector of length 1");
  }
  
  const char *filename;
  filename = CHAR(STRING_ELT(file, 0));
  TripletDistanceCalculator tripletCalc;

  std::vector<std::vector<INTTYPE_REST> > res = tripletCalc.calculateAllPairsTripletDistance(filename);

  IntegerMatrix IM_res(res.size(), res.size());
  
  for (size_t r = 0; r < res.size(); ++r) {
    for (size_t c = 0; c < r; ++c) {
      int current_res = int(res[r][c]);
      IM_res[r + res.size() * c] = current_res;
      IM_res[c + res.size() * r] = current_res;
    }
    IM_res[r + res.size()*r] = res[r][r];
  }
  
  return IM_res;
}
