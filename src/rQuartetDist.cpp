#include <Rcpp.h>
using namespace Rcpp;

#include "int_stuff.h"
#include "QuartetDistanceCalculator.h"

#include <vector>

//' Direct entry points to 'tqDist' functions
//' 
//' Functions to calculate triplet and quartet distances between pairs of trees.
//' Input is not checked for sanity.
//' 
//' Functions are called from R with user-friendly functions such as 
//' [`AllPairsQuartetDistance`].
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
IntegerVector tqdist_QuartetDistance(CharacterVector file1, CharacterVector file2) {
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
  IntegerVector IV_res(1);
  IV_res[0] = res;
  return IV_res;
}


//' @describeIn tqdist_QuartetDistance Agreement of each quartet
//' @export
// [[Rcpp::export]]
IntegerVector tqdist_QuartetAgreement(CharacterVector file1, CharacterVector file2) {
  int n1 = file1.size(), n2 = file2.size();
  if (n1 != 1 || n2 != 1) {
    Rcpp::stop("file1 and file2 must be character vectors of length 1");
  }
  
  const char *filename1;
  const char *filename2;
  
  filename1 = CHAR(STRING_ELT(file1, 0));
  filename2 = CHAR(STRING_ELT(file2, 0));
  
  QuartetDistanceCalculator quartetCalc;
  AE counts = quartetCalc.calculateQuartetAgreement(filename1, filename2);
  
  IntegerVector IV_res(2);
  IV_res[0] = counts.a;
  IV_res[1] = counts.e;
  
  return IV_res;
}

//' @describeIn tqdist_QuartetDistance Agreement of each quartet
//' @export
// [[Rcpp::export]]
IntegerVector tqdist_QuartetAgreementEdge(IntegerMatrix edge1,
                                          IntegerMatrix edge2) {
  QuartetDistanceCalculator quartetCalc;
  AE counts = quartetCalc.calculateQuartetAgreement(edge1, edge2);
  
  IntegerVector IV_res(2);
  IV_res[0] = counts.a;
  IV_res[1] = counts.e;
  
  return IV_res;
}

//' @describeIn tqdist_QuartetDistance Agreement of each quartet
//' @export
// [[Rcpp::export]]
IntegerVector tqdist_QuartetAgreementChar(CharacterVector string1,
                                          CharacterVector string2) {
  QuartetDistanceCalculator quartetCalc;
  AE counts = quartetCalc.calculateQuartetAgreement(string1, string2);
  
  IntegerVector IV_res(2);
  IV_res[0] = counts.a;
  IV_res[1] = counts.e;
  
  return IV_res;
}

//' @describeIn tqdist_QuartetDistance Distance between pairs
//' @export
// [[Rcpp::export]]
IntegerVector tqdist_PairsQuartetDistance(CharacterVector file1,
                                          CharacterVector file2) {
  int n1 = file1.size(), n2 = file2.size();
  if (n1 != 1 || n2 != 1) {
    Rcpp::stop("file1 and file2 must be character vectors of length 1");
  }
  
  const char *filename1;
  const char *filename2;
  
  filename1 = CHAR(STRING_ELT(file1, 0));
  filename2 = CHAR(STRING_ELT(file2, 0));
  
  QuartetDistanceCalculator quartetCalc;
  
  std::vector<INTTYPE_N4> res = 
    quartetCalc.pairs_quartet_distance(filename1, filename2);
  
  IntegerVector IV_res(res.size());
  for (size_t i = 0; i < res.size(); i++) {
    IV_res[i] = res[i];
  }
  return IV_res;
}

//' @describeIn tqdist_QuartetDistance Distance between pairs
//' @export
// [[Rcpp::export]]
IntegerVector tqdist_OneToManyQuartetAgreement(CharacterVector file1,
                                               CharacterVector fileMany) {
  int n1 = file1.size(), n2 = fileMany.size();
  if (n1 != 1 || n2 != 1) {
    Rcpp::stop("file1 and file2 must be character vectors of length 1");
  }
  
  const char *fileSingle;
  const char *fileMultiple;
  
  fileSingle = CHAR(STRING_ELT(file1, 0));
  fileMultiple = CHAR(STRING_ELT(fileMany, 0));
  
  QuartetDistanceCalculator quartetCalc;
  
  return quartetCalc.oneToManyQuartetAgreement(fileSingle, fileMultiple);
}

//' @describeIn tqdist_QuartetDistance Distance between pairs
//' @export
// [[Rcpp::export]]
IntegerVector tqdist_OneToManyQuartetAgreementChar(CharacterVector tree, 
                                                   CharacterVector trees) {
  QuartetDistanceCalculator quartetCalc;
  
  return quartetCalc.oneToManyQuartetAgreement(tree, trees);
}

//' @describeIn tqdist_QuartetDistance Distance between pairs
//' @export
// [[Rcpp::export]]
IntegerVector tqdist_OneToManyQuartetAgreementEdge(IntegerMatrix edge, 
                                                   ListOf<IntegerMatrix> edges) {
  QuartetDistanceCalculator quartetCalc;
  
  return quartetCalc.oneToManyQuartetAgreement(edge, edges);
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
  
  std::vector<std::vector<INTTYPE_N4> > res = 
    quartetCalc.calculateAllPairsQuartetDistance(filename);
  
  IntegerMatrix IM_res(res.size(), res.size());
  
  for (size_t r = 0; r < res.size(); r++) {
    for (size_t c = 0; c < r; c++) {
      int current_res = int(res[r][c]);
      IM_res[r + res.size() * c] = current_res;
      IM_res[c + res.size() * r] = current_res;
    }
    IM_res[r + res.size()*r] = res[r][r];
  }
  
  return IM_res;
}

//' @describeIn tqdist_QuartetDistance Distance between all pairs
//' @export
// [[Rcpp::export]]
IntegerMatrix tqdist_AllPairsQuartetDistanceChar(CharacterVector string) {
  QuartetDistanceCalculator quartetCalc;
  
  std::vector<std::vector<INTTYPE_N4> > res =
    quartetCalc.calculateAllPairsQuartetDistance(string);
  
  IntegerMatrix IM_res(res.size(), res.size());
  
  for (size_t r = 0; r < res.size(); r++) {
    for (size_t c = 0; c < r; c++) {
      int current_res = int(res[r][c]);
      IM_res[r + res.size() * c] = current_res;
      IM_res[c + res.size() * r] = current_res;
    }
    IM_res[r + res.size()*r] = res[r][r];
  }
  
  return IM_res;
}

//' @describeIn tqdist_QuartetDistance Distance between all pairs
//' @export
// [[Rcpp::export]]
IntegerMatrix tqdist_AllPairsQuartetDistanceEdge(ListOf<IntegerMatrix> edges) {
  QuartetDistanceCalculator quartetCalc;
  
  std::vector<std::vector<INTTYPE_N4> > res =
    quartetCalc.calculateAllPairsQuartetDistance(edges);
  
  IntegerMatrix IM_res(res.size(), res.size());
  
  for (size_t r = 0; r < res.size(); r++) {
    for (size_t c = 0; c < r; c++) {
      int current_res = int(res[r][c]);
      IM_res[r + res.size() * c] = current_res;
      IM_res[c + res.size() * r] = current_res;
    }
    IM_res[r + res.size()*r] = res[r][r];
  }
  
  return IM_res;
}

//' @describeIn tqdist_QuartetDistance Agreement between all pairs of trees
//' @export
// [[Rcpp::export]]
IntegerMatrix tqdist_AllPairsQuartetAgreement(CharacterVector file) {
  int n = file.size();
  if (n != 1) {
    Rcpp::stop("file must be a character vector of length 1");
  }
  
  const char *filename;
  filename = CHAR(STRING_ELT(file, 0));
  QuartetDistanceCalculator quartetCalc;
  
  std::vector<std::vector<std::vector<INTTYPE_N4> > > res = 
    quartetCalc.calculateAllPairsQuartetAgreement(filename);
  
  IntegerMatrix IM_res(res.size(), res.size() * 2);

  for (size_t r = 0; r < res.size(); r++) {
    for (size_t c = 0; c <= r; c++) {
      int current_a = int(res[r][c][0]);
      int current_e = int(res[r][c][1]);
      IM_res[r + res.size() * c] = current_a;
      IM_res[c + res.size() * r] = current_a;
      IM_res[r + res.size() * c + (res.size() * res.size())] = current_e;
      IM_res[c + res.size() * r + (res.size() * res.size())] = current_e;
    }
  }
  
  return IM_res;
}

//' @describeIn tqdist_QuartetDistance Agreement between all pairs of trees
//' @export
// [[Rcpp::export]]
IntegerMatrix tqdist_AllPairsQuartetAgreementChar(CharacterVector string) {
  QuartetDistanceCalculator quartetCalc;
  
  std::vector<std::vector<std::vector<INTTYPE_N4> > > res = 
    quartetCalc.calculateAllPairsQuartetAgreement(string);
  
  IntegerMatrix IM_res(res.size(), res.size() * 2);
  
  for (size_t r = 0; r < res.size(); r++) {
    for (size_t c = 0; c <= r; c++) {
      int current_a = int(res[r][c][0]);
      int current_e = int(res[r][c][1]);
      IM_res[r + res.size() * c] = current_a;
      IM_res[c + res.size() * r] = current_a;
      IM_res[r + res.size() * c + (res.size() * res.size())] = current_e;
      IM_res[c + res.size() * r + (res.size() * res.size())] = current_e;
    }
  }
  
  return IM_res;
}

//' @describeIn tqdist_QuartetDistance Agreement between all pairs of trees
//' @export
// [[Rcpp::export]]
IntegerMatrix tqdist_AllPairsQuartetAgreementEdge(ListOf<IntegerMatrix> edges) {
  QuartetDistanceCalculator quartetCalc;
  
  std::vector<std::vector<std::vector<INTTYPE_N4> > > res = 
    quartetCalc.calculateAllPairsQuartetAgreement(edges);
  
  IntegerMatrix IM_res(res.size(), res.size() * 2);
  
  for (size_t r = 0; r < res.size(); r++) {
    for (size_t c = 0; c <= r; c++) {
      int current_a = int(res[r][c][0]);
      int current_e = int(res[r][c][1]);
      IM_res[r + res.size() * c] = current_a;
      IM_res[c + res.size() * r] = current_a;
      IM_res[r + res.size() * c + (res.size() * res.size())] = current_e;
      IM_res[c + res.size() * r + (res.size() * res.size())] = current_e;
    }
  }
  
  return IM_res;
}

// Count resolved/unresolved quartets for a single preordered tree.
// Implements the algorithm of Brodal et al. (2013), matching the R-level
// ResolvedQuartets() but avoiding R loop overhead.
//
// edge: integer matrix with columns [parent, child] (1-indexed, preorder)
// nTip: number of tips in the tree
//
// Returns IntegerVector of length 2: [resolved, unresolved].
// [[Rcpp::export]]
IntegerVector resolved_quartets(IntegerMatrix edge, int nTip) {
  int nEdge = edge.nrow();

  // Internal nodes are numbered nTip+1 .. nTip+nNode
  int maxParent = 0;
  for (int i = 0; i < nEdge; i++) {
    int p = edge(i, 0);
    if (p > maxParent) maxParent = p;
  }
  int nNode = maxParent - nTip;

  // Build children lists: childStart[i] and degree[i] index into childBuf
  // for internal node nTip + 1 + i (0-indexed i).
  std::vector<int> degree(nNode, 0);
  for (int i = 0; i < nEdge; i++) {
    degree[edge(i, 0) - nTip - 1]++;
  }
  std::vector<int> childStart(nNode);
  int total = 0;
  for (int i = 0; i < nNode; i++) {
    childStart[i] = total;
    total += degree[i];
  }
  std::vector<int> childBuf(total);
  std::vector<int> pos(nNode, 0);
  for (int i = 0; i < nEdge; i++) {
    int idx = edge(i, 0) - nTip - 1;
    childBuf[childStart[idx] + pos[idx]] = edge(i, 1);
    pos[idx]++;
  }

  // n[v] = subtree size (1-indexed, node v in 1..nTip+nNode)
  std::vector<int> n(nTip + nNode + 1, 1);

  double totalUnresolved = 0.0;

  // Process internal nodes in reverse order (post-order for preorder trees)
  for (int nodeIdx = nNode - 1; nodeIdx >= 0; nodeIdx--) {
    int node = nTip + 1 + nodeIdx;
    int cStart = childStart[nodeIdx];
    int cEnd = cStart + degree[nodeIdx];

    int subtreeSize = 0;
    for (int ci = cStart; ci < cEnd; ci++) {
      subtreeSize += n[childBuf[ci]];
    }
    n[node] = subtreeSize;

    // Brodal et al. (2013) counting
    double s_vi = n[childBuf[cStart]];
    double p_vi = 0, t_vi = 0, q_vi = 0;
    for (int ci = cStart + 1; ci < cEnd; ci++) {
      double n_vi = n[childBuf[ci]];
      q_vi += n_vi * t_vi;
      t_vi += n_vi * p_vi;
      p_vi += n_vi * s_vi;
      s_vi += n_vi;
    }
    totalUnresolved += q_vi + t_vi * (nTip - s_vi);
  }

  double nTipD = (double)nTip;
  double totalQuartets = nTipD * (nTipD - 1) * (nTipD - 2) * (nTipD - 3) / 24.0;
  double resolved = totalQuartets - totalUnresolved;

  double intMax = (double)INT_MAX;
  if (resolved > intMax || totalUnresolved > intMax) {
    Rcpp::stop("Sorry: trees too large for integer representation");
  }

  IntegerVector result(2);
  result[0] = (int)resolved;
  result[1] = (int)totalUnresolved;
  return result;
}
