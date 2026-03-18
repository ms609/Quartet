#include "QuartetDistanceCalculator.h"
#include "int_stuff.h"

#include <Rcpp.h>
using namespace Rcpp;
#include "hdt.h"
#include "hdt_factory.h"
#include "newick_parser.h"
#include "edge_parser.h"
#include "unrooted_tree.h"
#include "rooted_tree.h"
#include "int_stuff.h"

#include <cmath>
#include <cstdlib>
#include <memory>
#include <string>
#ifdef _OPENMP
#include <omp.h>
#endif

// RAII guard: deletes all UnrootedTree pointers on destruction.
struct TreeVecGuard {
  std::vector<UnrootedTree *> trees;
  TreeVecGuard() = default;
  ~TreeVecGuard() {
    for (size_t i = trees.size(); i--; ) delete trees[i];
  }
  TreeVecGuard(const TreeVecGuard &) = delete;
  TreeVecGuard &operator=(const TreeVecGuard &) = delete;
};

QuartetDistanceCalculator::QuartetDistanceCalculator() {
  dummyHDTFactory = new HDTFactory(0);
  dummyRTFactory  = new RootedTreeFactory();
}

QuartetDistanceCalculator::~QuartetDistanceCalculator() {
  delete dummyHDTFactory;
  delete dummyRTFactory;
}

Rcpp::IntegerVector QuartetDistanceCalculator::oneToManyQuartetAgreement\
  (UnrootedTree *unrootedSingle, std::vector<UnrootedTree *> &unrootedMultiple) {
  const int n = (int)unrootedMultiple.size();
  // Plain vectors for thread-safe parallel writes (Rcpp vectors are not safe).
  std::vector<INTTYPE_N4> resA(n, 0);
  std::vector<INTTYPE_N4> resE(n, 0);

  volatile bool hasError = false;
  std::string errorMsg;

#ifdef _OPENMP
  #pragma omp parallel
#endif
  {
    // One calculator per thread — member state is not re-entrant.
    QuartetDistanceCalculator localCalc;
#ifdef _OPENMP
    #pragma omp for schedule(dynamic, 1)
#endif
    for (int i = 0; i < n; ++i) {
      if (hasError) continue;
      try {
        AE status = localCalc.calculateQuartetAgreement(
          unrootedSingle, unrootedMultiple[i]);
        resA[i] = status.a;
        resE[i] = status.e;
      } catch (const std::exception& e) {
#ifdef _OPENMP
        #pragma omp critical
#endif
        {
          if (!hasError) {
            hasError = true;
            errorMsg = e.what();
          }
        }
      }
    }
  }

  if (hasError) Rcpp::stop("%s", errorMsg.c_str());

  Rcpp::IntegerVector res(n * 2);
  for (int i = 0; i < n; ++i) {
    res[i] = (int)resA[i];
    res[i + n] = (int)resE[i];
  }
  return res;
}

Rcpp::IntegerVector QuartetDistanceCalculator::oneToManyQuartetAgreement\
  (const char *fileSingle, const char *fileMultiple) {
  
  NewickParser parser;
  
  std::unique_ptr<UnrootedTree> unrootedSingle(parser.parseFile(fileSingle));
  if (!unrootedSingle || parser.isError()) {
    Rcpp::stop("Error parsing fileSingle in oneToManyQuartets -> parser.parseFile");
  }

  TreeVecGuard multiGuard;
  multiGuard.trees = parser.parseMultiFile(fileMultiple);
  if (multiGuard.trees.size() == 0) {
    Rcpp::stop("No trees found in fileMultiple; does file end with blank line?");
  } else if (parser.isError()) {
    Rcpp::stop("Error parsing fileMultiple in oneToManyQuartetAgreement -> parser.parseFile");
  }

  return oneToManyQuartetAgreement(unrootedSingle.get(), multiGuard.trees);
}

Rcpp::IntegerVector QuartetDistanceCalculator::oneToManyQuartetAgreement\
  (CharacterVector tree, CharacterVector trees) {
  NewickParser parser;
  
  std::unique_ptr<UnrootedTree> unrootedSingle(parser.parseStr(tree));
  if (!unrootedSingle || parser.isError()) {
    Rcpp::stop("Error parsing tree in oneToManyQuartets -> parser.parseFile");
  }

  TreeVecGuard multiGuard;
  multiGuard.trees = parser.parseMultiStr(trees);
  if (multiGuard.trees.size() == 0) {
    Rcpp::stop("No trees found in trees");
  } else if (parser.isError()) {
    Rcpp::stop("Error parsing trees in oneToManyQuartetAgreement -> parser.parseFile");
  }

  return oneToManyQuartetAgreement(unrootedSingle.get(), multiGuard.trees);
}

Rcpp::IntegerVector QuartetDistanceCalculator::oneToManyQuartetAgreement\
  (IntegerMatrix edge, ListOf<IntegerMatrix> edges) {
  EdgeParser parser;
  
  std::unique_ptr<UnrootedTree> unrootedSingle(parser.parseEdge(edge));
  if (!unrootedSingle) {
    Rcpp::stop("Error parsing tree in oneToManyQuartets -> parser.parseFile");
  }

  TreeVecGuard multiGuard;
  multiGuard.trees = parser.parseEdges(edges);
  if (multiGuard.trees.size() == 0) {
    Rcpp::stop("No trees found in trees");
  }

  return oneToManyQuartetAgreement(unrootedSingle.get(), multiGuard.trees);
}

std::vector<INTTYPE_N4> QuartetDistanceCalculator::\
  pairs_quartet_distance(std::vector<UnrootedTree *> &unrootedTrees1,
                         std::vector<UnrootedTree *> &unrootedTrees2) {
  const int nTrees = (int)unrootedTrees1.size();
  std::vector<INTTYPE_N4> res(nTrees, INTTYPE_N4(0));

  volatile bool hasError = false;
  std::string errorMsg;

#ifdef _OPENMP
  #pragma omp parallel
#endif
  {
    QuartetDistanceCalculator localCalc;
#ifdef _OPENMP
    #pragma omp for schedule(dynamic, 1)
#endif
    for (int i = 0; i < nTrees; ++i) {
      if (hasError) continue;
      try {
        res[i] = localCalc.calculateQuartetDistance(
          unrootedTrees1[i], unrootedTrees2[i]);
      } catch (const std::exception& e) {
#ifdef _OPENMP
        #pragma omp critical
#endif
        {
          if (!hasError) {
            hasError = true;
            errorMsg = e.what();
          }
        }
      }
    }
  }

  if (hasError) Rcpp::stop("%s", errorMsg.c_str());

  return res;
}

std::vector<INTTYPE_N4> QuartetDistanceCalculator::\
  pairs_quartet_distance(const char *filename1, const char *filename2) {
  NewickParser parser;
  
  TreeVecGuard guard1;
  guard1.trees = parser.parseMultiFile(filename1);
  if (guard1.trees.size() == 0) {
    Rcpp::stop("No trees found in filename1; does file end with blank line?");
  } else if (parser.isError()) {
    Rcpp::stop("Error parsing filename1 in pairs_quartet_distance -> parser.parseMultiFile");
  }

  TreeVecGuard guard2;
  guard2.trees = parser.parseMultiFile(filename2);
  if (guard2.trees.size() == 0) {
    Rcpp::stop("No trees found in filename2; does file end with blank line?");
  } else if (parser.isError()) {
    Rcpp::stop("Error parsing filename2 in pairs_quartet_distance -> parser.parseMultiFile");
  }

  return pairs_quartet_distance(guard1.trees, guard2.trees);
}


/*  Calculate All Pairs DISTANCE */
std::vector<std::vector<INTTYPE_N4> > QuartetDistanceCalculator::\
  calculateAllPairsQuartetDistance(const char *filename) {
  NewickParser parser;

  TreeVecGuard guard;
  guard.trees = parser.parseMultiFile(filename);
  if (guard.trees.size() == 0 || parser.isError()) {
    Rcpp::stop("Error: Failed to parse filename");
  }

  return calculateAllPairsQuartetDistance(guard.trees);
}

std::vector<std::vector<INTTYPE_N4> > QuartetDistanceCalculator::\
  calculateAllPairsQuartetDistance(CharacterVector string) {
  NewickParser parser;
  
  TreeVecGuard guard;
  guard.trees = parser.parseMultiStr(string);
  if (guard.trees.size() == 0 || parser.isError()) {
    Rcpp::stop("Error: Failed to parse input string");
  }

  return calculateAllPairsQuartetDistance(guard.trees);
}

std::vector<std::vector<INTTYPE_N4> > QuartetDistanceCalculator::\
  calculateAllPairsQuartetDistance(ListOf<IntegerMatrix> edges) {
  EdgeParser parser;
  
  TreeVecGuard guard;
  guard.trees = parser.parseEdges(edges);
  if (guard.trees.size() == 0) {
    Rcpp::stop("Error: Failed to parse input edges");
  }

  return calculateAllPairsQuartetDistance(guard.trees);
}

std::vector<std::vector<INTTYPE_N4> > QuartetDistanceCalculator::\
  calculateAllPairsQuartetDistance(std::vector<UnrootedTree *> trees) {
  const int nTrees = (int)trees.size();
  std::vector<std::vector<INTTYPE_N4> > results(nTrees);

  // Pre-size rows so threads can write by index without push_back.
  // Row r: [dist(r,0),...,dist(r,r-1),0(diagonal)] - same layout as before.
  for (int r = 0; r < nTrees; ++r) {
    results[r].assign(r + 1, INTTYPE_N4(0));
  }

  // Flatten the strict lower triangle to a single linear index
  // k = r*(r-1)/2 + c  where  0 <= c < r < nTrees.
  // Inversion: r = floor((1 + sqrt(1 + 8k)) / 2),  c = k - r*(r-1)/2.
  const int nPairs = nTrees * (nTrees - 1) / 2;

  volatile bool hasError = false;
  std::string errorMsg;

#ifdef _OPENMP
  #pragma omp parallel
#endif
  {
    // One calculator per thread — member state is not re-entrant.
    QuartetDistanceCalculator localCalc;
#ifdef _OPENMP
    #pragma omp for schedule(dynamic, 1)
#endif
    for (int k = 0; k < nPairs; ++k) {
      if (hasError) continue;
      try {
        int r = (int)((1.0 + std::sqrt(1.0 + 8.0 * k)) / 2.0);
        if (r * (r - 1) / 2 > k) --r;  // guard against float rounding
        const int c = k - r * (r - 1) / 2;
        results[r][c] = localCalc.calculateQuartetDistance(trees[r], trees[c]);
        // results[r][r] remains 0 from assign() above.
      } catch (const std::exception& e) {
#ifdef _OPENMP
        #pragma omp critical
#endif
        {
          if (!hasError) {
            hasError = true;
            errorMsg = e.what();
          }
        }
      }
    }
  }

  if (hasError) Rcpp::stop("%s", errorMsg.c_str());

  return results;
}

/* Calculate All Pairs AGREEMENT */
std::vector<std::vector<std::vector<INTTYPE_N4> > > QuartetDistanceCalculator::\
    calculateAllPairsQuartetAgreement(const char *filename) {
  
  NewickParser parser;
  
  TreeVecGuard guard;
  guard.trees = parser.parseMultiFile(filename);
  if (guard.trees.size() == 0 || parser.isError()) {
    Rcpp::stop("Error: Failed to parse filename");
  }
  
  return calculateAllPairsQuartetAgreement(guard.trees);
}

std::vector<std::vector<std::vector<INTTYPE_N4> > > QuartetDistanceCalculator::\
    calculateAllPairsQuartetAgreement(CharacterVector string) {
  
  NewickParser parser;
  
  TreeVecGuard guard;
  guard.trees = parser.parseMultiStr(string);
  if (guard.trees.size() == 0 || parser.isError()) {
    Rcpp::stop("Error: Failed to parse input string");
  }
  
  return calculateAllPairsQuartetAgreement(guard.trees);
}

std::vector<std::vector<std::vector<INTTYPE_N4> > > QuartetDistanceCalculator::\
    calculateAllPairsQuartetAgreement(ListOf<IntegerMatrix> edges) {
  
  EdgeParser parser;
  
  TreeVecGuard guard;
  guard.trees = parser.parseEdges(edges);
  if (guard.trees.size() == 0) {
    Rcpp::stop("Error: Failed to parse input edges");
  }
  
  return calculateAllPairsQuartetAgreement(guard.trees);
}

std::vector<std::vector<std::vector<INTTYPE_N4> > > QuartetDistanceCalculator::\
  calculateAllPairsQuartetAgreement(std::vector<UnrootedTree *> trees) {
  const int nTrees = (int)trees.size();
  std::vector<std::vector<std::vector<INTTYPE_N4> > > results(nTrees);

  // Pre-size: row r has r+1 elements including diagonal self-comparison.
  for (int r = 0; r < nTrees; ++r) {
    results[r].assign(r + 1, std::vector<INTTYPE_N4>(2, INTTYPE_N4(0)));
  }

  // Flatten the lower triangle (including diagonal) to a single linear index
  // k = r*(r+1)/2 + c  where  0 <= c <= r < nTrees.
  // Inversion: r = floor((-1 + sqrt(1 + 8k)) / 2),  c = k - r*(r+1)/2.
  const int nPairs = nTrees * (nTrees + 1) / 2;

  volatile bool hasError = false;
  std::string errorMsg;

#ifdef _OPENMP
  #pragma omp parallel
#endif
  {
    // One calculator per thread — member state is not re-entrant.
    QuartetDistanceCalculator localCalc;
#ifdef _OPENMP
    #pragma omp for schedule(dynamic, 1)
#endif
    for (int k = 0; k < nPairs; ++k) {
      if (hasError) continue;
      try {
        int r = (int)((-1.0 + std::sqrt(1.0 + 8.0 * k)) / 2.0);
        if (r * (r + 1) / 2 > k) --r;  // guard against float rounding
        const int c = k - r * (r + 1) / 2;
        // Self-comparison (c==r) gives total quartet count split.
        AE counts = localCalc.calculateQuartetAgreement(trees[r], trees[c]);
        results[r][c][0] = counts.a;
        results[r][c][1] = counts.e;
      } catch (const std::exception& e) {
#ifdef _OPENMP
        #pragma omp critical
#endif
        {
          if (!hasError) {
            hasError = true;
            errorMsg = e.what();
          }
        }
      }
    }
  }

  if (hasError) Rcpp::stop("%s", errorMsg.c_str());

  return results;
}
AE QuartetDistanceCalculator::\
  calculateQuartetAgreement(const char *filename1, const char *filename2) {
  NewickParser parser;

  std::unique_ptr<UnrootedTree> ut1(parser.parseFile(filename1));
  if (!ut1 || parser.isError()) {
    Rcpp::stop("calculateQuartetDistance failed to parse filename1");
  }

  std::unique_ptr<UnrootedTree> ut2(parser.parseFile(filename2));
  if (!ut2 || parser.isError()) {
    Rcpp::stop("calculateQuartetDistance failed to parse filename2");
  }

  return calculateQuartetAgreement(ut1.get(), ut2.get());
}

AE QuartetDistanceCalculator::\
  calculateQuartetAgreement(CharacterVector t1, CharacterVector t2) {
  NewickParser parser;
  
  std::unique_ptr<UnrootedTree> ut1(parser.parseStr(t1));
  if (!ut1 || parser.isError()) {
    Rcpp::stop("calculateQuartetDistance failed to parse filename1");
  }

  std::unique_ptr<UnrootedTree> ut2(parser.parseStr(t2));
  if (!ut2 || parser.isError()) {
    Rcpp::stop("calculateQuartetDistance failed to parse filename2");
  }

  return calculateQuartetAgreement(ut1.get(), ut2.get());
}

AE QuartetDistanceCalculator::\
  calculateQuartetAgreement(IntegerMatrix edge1, IntegerMatrix edge2) {
  EdgeParser parser;

  std::unique_ptr<UnrootedTree> ut1(parser.parseEdge(edge1));
  if (!ut1) {
    Rcpp::stop("calculateQuartetDistance failed to parse edge1");
  }

  std::unique_ptr<UnrootedTree> ut2(parser.parseEdge(edge2));
  if (!ut2) {
    Rcpp::stop("calculateQuartetDistance failed to parse edge2");
  }

  return calculateQuartetAgreement(ut1.get(), ut2.get());
}

AE QuartetDistanceCalculator::\
  calculateQuartetAgreement(UnrootedTree *t1, UnrootedTree *t2) {

  AE res;
  UnrootedTree *tmp;
  if(t1->maxDegree > t2->maxDegree) { // Smallest degree tree as t1
    tmp = t1;
    t1 = t2;
    t2 = tmp;
  }

  this->t1 = t1->convertToRootedTree(dummyRTFactory);
  this->t2 = t2->convertToRootedTree(this->t1->factory);
  
  this->t1->pairAltWorld(this->t2);
  if (this->t1->isError()) {
    Rcpp::stop("The two trees do not have the same set of leaves.");
    res.a = -1;
    res.e = -1;
    res.noQuartets = -1;
    return res;
  }
  
  // tqDist comment asserts that countChildren corresponds to 
  // Section 3 of Brodal et al. 2013:
  // Counting unresolved triplets and quartets in a single tree
  
  // Populate this->t1->n with the number of leaves
  countChildren(this->t1);
  
  
  // HDT: Heirarchical Decomposition Tree
  // See Section 4 in Brodal et al. 2013
  hdt = HDT::constructHDT(this->t2, this->t1->maxDegree, dummyHDTFactory);
  
  resolvedQuartetsAgree = resolvedQuartetsAgreeDiag = 0;
  resolvedQuartetsDisagree = resolvedQuartetsDisagreeDiag = 0;
  resolvedQuartetsAgreeUpper = resolvedQuartetsDisagreeUpper = 0;
  unresolvedQuartets = 0;
  
  count(this->t1);

  n = this->t1->n;
  
  res.a = resolvedQuartetsAgree + resolvedQuartetsAgreeDiag + resolvedQuartetsAgreeUpper;
  res.e = unresolvedQuartets;
  res.noQuartets = Util::binom4(n);
  
  // HDT is deleted in count
  delete this->t1->factory;
  delete this->t2->factory;
  
  return res;
}

INTTYPE_N4 QuartetDistanceCalculator::\
  calculateQuartetDistance(const char *filename1, const char *filename2) {
  NewickParser parser;

  std::unique_ptr<UnrootedTree> ut1(parser.parseFile(filename1));
  if (!ut1 || parser.isError()) {
    Rcpp::stop("calculateQuartetDistance failed to parse filename1");
  }

  std::unique_ptr<UnrootedTree> ut2(parser.parseFile(filename2));
  if (!ut2 || parser.isError()) {
    Rcpp::stop("calculateQuartetDistance failed to parse filename2");
  }

  return calculateQuartetDistance(ut1.get(), ut2.get());
}

INTTYPE_N4 QuartetDistanceCalculator::\
  calculateQuartetDistance(CharacterVector t1, CharacterVector t2) {
  NewickParser parser;

  std::unique_ptr<UnrootedTree> ut1(parser.parseStr(t1));
  if (!ut1 || parser.isError()) {
    Rcpp::stop("calculateQuartetDistance failed to parse filename1");
  }

  std::unique_ptr<UnrootedTree> ut2(parser.parseStr(t2));
  if (!ut2 || parser.isError()) {
    Rcpp::stop("calculateQuartetDistance failed to parse filename2");
  }

  return calculateQuartetDistance(ut1.get(), ut2.get());
}

INTTYPE_N4 QuartetDistanceCalculator::\
  calculateQuartetDistance(IntegerMatrix t1, IntegerMatrix t2) {
  AE ae = calculateQuartetAgreement(t1, t2);
  INTTYPE_N4 result = ae.noQuartets - (ae.a + ae.e);
  return result;
}

INTTYPE_N4 QuartetDistanceCalculator::\
  calculateQuartetDistance(UnrootedTree *t1, UnrootedTree *t2) {
  AE ae = calculateQuartetAgreement(t1, t2);
  INTTYPE_N4 result = ae.noQuartets - (ae.a + ae.e);
  return result;
}

void QuartetDistanceCalculator::updateCounters() {
  resolvedQuartetsAgree += hdt->quartResolvedAgree;
  resolvedQuartetsAgreeDiag += hdt->quartResolvedAgreeDiag;
  resolvedQuartetsAgreeUpper += hdt->quartResolvedAgreeUpper;
  unresolvedQuartets += hdt->quartSumE;
}
