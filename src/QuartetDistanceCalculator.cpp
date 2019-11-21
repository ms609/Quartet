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

#include <cstdlib>

QuartetDistanceCalculator::QuartetDistanceCalculator() {
  dummyHDTFactory = new HDTFactory(0);
}

QuartetDistanceCalculator::~QuartetDistanceCalculator() {
  delete dummyHDTFactory;
}

Rcpp::IntegerVector QuartetDistanceCalculator::oneToManyQuartetAgreement\
  (UnrootedTree *unrootedSingle, std::vector<UnrootedTree *> &unrootedMultiple) {
  Rcpp::IntegerVector res(unrootedMultiple.size() * 2);
  
  for(size_t i = 0; i < unrootedMultiple.size(); i++) {
    AE status = calculateQuartetAgreement(unrootedSingle, unrootedMultiple[i]);
    res[i] = status.a;
    res[i + unrootedMultiple.size()] = status.e;
  }
  
  return res;
}

Rcpp::IntegerVector QuartetDistanceCalculator::oneToManyQuartetAgreement\
  (const char *fileSingle, const char *fileMultiple) {
  
  NewickParser parser;
  
  UnrootedTree *unrootedSingle = parser.parseFile(fileSingle); 
  
  if (unrootedSingle == NULL || parser.isError()) {
    Rcpp::stop("Error parsing fileSingle in oneToManyQuartets -> parser.parseFile");
  }

  std::vector<UnrootedTree *> unrootedMultiple = parser.parseMultiFile(fileMultiple); 
  if (unrootedMultiple.size() == 0) {
    Rcpp::stop("No trees found in fileMultiple; does file end with blank line?");
  } else if (parser.isError()) {
    Rcpp::stop("Error parsing fileMultiple in oneToManyQuartetAgreement -> parser.parseFile");
  }

  return oneToManyQuartetAgreement(unrootedSingle, unrootedMultiple);
}

Rcpp::IntegerVector QuartetDistanceCalculator::oneToManyQuartetAgreement\
  (CharacterVector tree, CharacterVector trees) {
  NewickParser parser;
  
  UnrootedTree *unrootedSingle = parser.parseStr(tree); 
  
  if (unrootedSingle == NULL || parser.isError()) {
    Rcpp::stop("Error parsing tree in oneToManyQuartets -> parser.parseFile");
  }

  std::vector<UnrootedTree *> unrootedMultiple = parser.parseMultiStr(trees); 
  if (unrootedMultiple.size() == 0) {
    Rcpp::stop("No trees found in trees");
  } else if (parser.isError()) {
    Rcpp::stop("Error parsing trees in oneToManyQuartetAgreement -> parser.parseFile");
  }

  return oneToManyQuartetAgreement(unrootedSingle, unrootedMultiple);
}

Rcpp::IntegerVector QuartetDistanceCalculator::oneToManyQuartetAgreement\
  (IntegerMatrix edge, ListOf<IntegerMatrix> edges) {
  EdgeParser parser;
  
  UnrootedTree *unrootedSingle = parser.parseEdge(edge); 
  
  if (unrootedSingle == NULL) {
    Rcpp::stop("Error parsing tree in oneToManyQuartets -> parser.parseFile");
  }

  std::vector<UnrootedTree *> unrootedMultiple = parser.parseEdges(edges); 
  if (unrootedMultiple.size() == 0) {
    Rcpp::stop("No trees found in trees");
  }

  return oneToManyQuartetAgreement(unrootedSingle, unrootedMultiple);
}

std::vector<INTTYPE_N4> QuartetDistanceCalculator::\
  pairs_quartet_distance(std::vector<UnrootedTree *> &unrootedTrees1,
                         std::vector<UnrootedTree *> &unrootedTrees2) {
  std::vector<INTTYPE_N4> res;

  for(size_t i = 0; i < unrootedTrees1.size(); i++) {
    INTTYPE_N4 dist = calculateQuartetDistance(unrootedTrees1[i], unrootedTrees2[i]);
    
    res.push_back(dist);
  }

  return res;
}

std::vector<INTTYPE_N4> QuartetDistanceCalculator::\
  pairs_quartet_distance(const char *filename1, const char *filename2) {
  NewickParser parser;
  
  std::vector<UnrootedTree *> unrootedTrees1  = parser.parseMultiFile(filename1); 
  if (unrootedTrees1.size() == 0) {
    Rcpp::stop("No trees found in filename1; does file end with blank line?");
  } else if (parser.isError()) {
    Rcpp::stop("Error parsing filename1 in pairs_quartet_distance -> parser.parseMultiFile");
  }

  std::vector<UnrootedTree *> unrootedTrees2  = parser.parseMultiFile(filename2); 
  if (unrootedTrees2.size() == 0) {
    Rcpp::stop("No trees found in filename2; does file end with blank line?");
  } else if (parser.isError()) {
    Rcpp::stop("Error parsing filename2 in pairs_quartet_distance -> parser.parseMultiFile");
  }

  return pairs_quartet_distance(unrootedTrees1, unrootedTrees2);
}


/*  Calculate All Pairs DISTANCE */
std::vector<std::vector<INTTYPE_N4> > QuartetDistanceCalculator::\
  calculateAllPairsQuartetDistance(const char *filename) {
  NewickParser parser;

  std::vector<UnrootedTree *> unrootedTrees  = parser.parseMultiFile(filename); 
  if (unrootedTrees.size() == 0 || parser.isError()) {
    Rcpp::stop("Error: Failed to parse filename");
  }

  const std::vector<std::vector<INTTYPE_N4> > results =
    calculateAllPairsQuartetDistance(unrootedTrees);

  for(size_t i = 0; i < unrootedTrees.size(); ++i) {
    UnrootedTree * tmp = unrootedTrees[i];
    delete tmp;
  }

  return results;
}

std::vector<std::vector<INTTYPE_N4> > QuartetDistanceCalculator::\
  calculateAllPairsQuartetDistance(CharacterVector string) {
  NewickParser parser;
  
  std::vector<UnrootedTree *> unrootedTrees = parser.parseMultiStr(string); 
  if (unrootedTrees.size() == 0 || parser.isError()) {
    Rcpp::stop("Error: Failed to parse input string");
  }

  const std::vector<std::vector<INTTYPE_N4> > results = 
    calculateAllPairsQuartetDistance(unrootedTrees);

  for(size_t i = 0; i < unrootedTrees.size(); ++i) {
    UnrootedTree * tmp = unrootedTrees[i];
    delete tmp;
  }

  return results;
}

std::vector<std::vector<INTTYPE_N4> > QuartetDistanceCalculator::\
  calculateAllPairsQuartetDistance(ListOf<IntegerMatrix> edges) {
  EdgeParser parser;
  
  std::vector<UnrootedTree *> unrootedTrees = parser.parseEdges(edges); 
  if (unrootedTrees.size() == 0) {
    Rcpp::stop("Error: Failed to parse input edges");
  }

  const std::vector<std::vector<INTTYPE_N4> > results = 
    calculateAllPairsQuartetDistance(unrootedTrees);

  for(size_t i = 0; i < unrootedTrees.size(); ++i) {
    UnrootedTree * tmp = unrootedTrees[i];
    delete tmp;
  }

  return results;
}

std::vector<std::vector<INTTYPE_N4> > QuartetDistanceCalculator::\
  calculateAllPairsQuartetDistance(std::vector<UnrootedTree *> trees) {
  std::vector<std::vector<INTTYPE_N4> > results(trees.size());

  for(size_t r = 0; r < trees.size(); ++r) {
    for(size_t c = 0; c < r; ++c) {
      INTTYPE_N4 distance = calculateQuartetDistance(trees[r], trees[c]);
      results[r].push_back(distance);
    }
    results[r].push_back(0);
  }

  return results;
}

/* Calculate All Pairs AGREEMENT */
std::vector<std::vector<std::vector<INTTYPE_N4> > > QuartetDistanceCalculator::\
    calculateAllPairsQuartetAgreement(const char *filename) {
  
  NewickParser parser;
  
  std::vector<UnrootedTree *> unrootedTrees = parser.parseMultiFile(filename);
  if (unrootedTrees.size() == 0 || parser.isError()) {
    Rcpp::stop("Error: Failed to parse filename");
  }
  
  const std::vector<std::vector<std::vector<INTTYPE_N4> > > results = 
    calculateAllPairsQuartetAgreement(unrootedTrees);
  
  for(size_t i = 0; i < unrootedTrees.size(); ++i) {
    UnrootedTree * tmp = unrootedTrees[i];
    delete tmp;
  }
  
  return results;
}

std::vector<std::vector<std::vector<INTTYPE_N4> > > QuartetDistanceCalculator::\
    calculateAllPairsQuartetAgreement(CharacterVector string) {
  
  NewickParser parser;
  
  std::vector<UnrootedTree *> unrootedTrees  = parser.parseMultiStr(string);
  if (unrootedTrees.size() == 0 || parser.isError()) {
    Rcpp::stop("Error: Failed to parse input string");
  }
  
  const std::vector<std::vector<std::vector<INTTYPE_N4> > > results = 
    calculateAllPairsQuartetAgreement(unrootedTrees);
  
  for(size_t i = 0; i < unrootedTrees.size(); ++i) {
    UnrootedTree * tmp = unrootedTrees[i];
    delete tmp;
  }
  
  return results;
}

std::vector<std::vector<std::vector<INTTYPE_N4> > > QuartetDistanceCalculator::\
    calculateAllPairsQuartetAgreement(ListOf<IntegerMatrix> edges) {
  
  EdgeParser parser;
  
  std::vector<UnrootedTree *> unrootedTrees  = parser.parseEdges(edges);
  if (unrootedTrees.size() == 0) {
    Rcpp::stop("Error: Failed to parse input edges");
  }
  
  const std::vector<std::vector<std::vector<INTTYPE_N4> > > results = 
    calculateAllPairsQuartetAgreement(unrootedTrees);
  
  for(size_t i = 0; i < unrootedTrees.size(); ++i) {
    UnrootedTree * tmp = unrootedTrees[i];
    delete tmp;
  }
  
  return results;
}

std::vector<std::vector<std::vector<INTTYPE_N4> > > QuartetDistanceCalculator::\
  calculateAllPairsQuartetAgreement(std::vector<UnrootedTree *> trees) {
  
  std::vector<std::vector<std::vector<INTTYPE_N4> > > results(trees.size());
  AE counts;
  
  for(size_t r = 0; r < trees.size(); ++r) {
    for(size_t c = 0; c <= r; ++c) {
      // comparing a tree with itself gives us (A+B+C) / (D+E)
      counts = calculateQuartetAgreement(trees[r], trees[c]);
      std::vector<INTTYPE_N4> ae(2);
      ae[0] = counts.a;
      ae[1] = counts.e;
      results[r].push_back(ae);
    }
  }

  return results;
}

AE QuartetDistanceCalculator::\
  calculateQuartetAgreement(const char *filename1, const char *filename2) {
  UnrootedTree *ut1 = NULL;
  UnrootedTree *ut2 = NULL;
  NewickParser parser;

  ut1 = parser.parseFile(filename1);
  if (ut1 == NULL || parser.isError()) {
    Rcpp::stop("calculateQuartetDistance failed to parse filename1");
  }

  ut2 = parser.parseFile(filename2);
  if(ut2 == NULL || parser.isError()) {
    Rcpp::stop("calculateQuartetDistance failed to parse filename2");
  }

  AE res = calculateQuartetAgreement(ut1, ut2);

  delete ut1;
  delete ut2;

  return res;
}

AE QuartetDistanceCalculator::\
  calculateQuartetAgreement(CharacterVector t1, CharacterVector t2) {
  UnrootedTree *ut1 = NULL;
  UnrootedTree *ut2 = NULL;
  NewickParser parser;
    
  ut1 = parser.parseStr(t1);
  if (ut1 == NULL || parser.isError()) {
    Rcpp::stop("calculateQuartetDistance failed to parse filename1");
  }

  ut2 = parser.parseStr(t2);
  if(ut2 == NULL || parser.isError()) {
    Rcpp::stop("calculateQuartetDistance failed to parse filename2");
  }

  AE res = calculateQuartetAgreement(ut1, ut2);

  delete ut1;
  delete ut2;

  return res;
}

AE QuartetDistanceCalculator::\
  calculateQuartetAgreement(IntegerMatrix edge1, IntegerMatrix edge2) {
  UnrootedTree *ut1 = NULL;
  UnrootedTree *ut2 = NULL;
  EdgeParser parser;

  ut1 = parser.parseEdge(edge1);
  if (ut1 == NULL) {
    Rcpp::stop("calculateQuartetDistance failed to parse edge1");
  }

  ut2 = parser.parseEdge(edge2);
  if(ut2 == NULL) {
    delete ut1;
    Rcpp::stop("calculateQuartetDistance failed to parse edge2");
  }

  AE res = calculateQuartetAgreement(ut1, ut2);

  delete ut1;
  delete ut2;

  return res;
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

  this->t1 = t1->convertToRootedTree(NULL);
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
  UnrootedTree *ut1 = NULL;
  UnrootedTree *ut2 = NULL;
  NewickParser parser;
  
  ut1 = parser.parseFile(filename1);
  if (ut1 == NULL || parser.isError()) {
    Rcpp::stop("calculateQuartetDistance failed to parse filename1");
  }
  
  ut2 = parser.parseFile(filename2);
  if(ut2 == NULL || parser.isError()) {
    Rcpp::stop("calculateQuartetDistance failed to parse filename2");
  }
  
  INTTYPE_N4 res = calculateQuartetDistance(ut1, ut2);
  
  delete ut1;
  delete ut2;
  
  return res;
}

INTTYPE_N4 QuartetDistanceCalculator::\
  calculateQuartetDistance(CharacterVector t1, CharacterVector t2) {
  UnrootedTree *ut1 = NULL;
  UnrootedTree *ut2 = NULL;
  NewickParser parser;
  
  ut1 = parser.parseStr(t1);
  if (ut1 == NULL || parser.isError()) {
    Rcpp::stop("calculateQuartetDistance failed to parse filename1");
  }
  
  ut2 = parser.parseStr(t2);
  if(ut2 == NULL || parser.isError()) {
    Rcpp::stop("calculateQuartetDistance failed to parse filename2");
  }
  
  INTTYPE_N4 res = calculateQuartetDistance(ut1, ut2);
  
  delete ut1;
  delete ut2;
  
  return res;
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
