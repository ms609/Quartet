#include "QuartetDistanceCalculator.h"
#include "int_stuff.h"

#include "hdt.h"
#include "hdt_factory.h"
#include "newick_parser.h"
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

std::vector<INTTYPE_N4> QuartetDistanceCalculator::pairs_quartet_distance(std::vector<UnrootedTree *> &unrootedTrees1, std::vector<UnrootedTree *> &unrootedTrees2) {
  std::vector<INTTYPE_N4> res;

  for(size_t i = 0; i < unrootedTrees1.size(); i++) {
    INTTYPE_N4 dist = calculateQuartetDistance(unrootedTrees1[i], unrootedTrees2[i]);
    
    res.push_back(dist);
  }

  return res;
}

std::vector<INTTYPE_N4> QuartetDistanceCalculator::pairs_quartet_distance(const char *filename1, const char *filename2) {
  NewickParser parser;
  
  std::vector<UnrootedTree *> unrootedTrees1  = parser.parseMultiFile(filename1); 
  if (unrootedTrees1.size() == 0 || parser.isError()) {
    std::cerr << "Error: Parsing of \"" << filename1 << "\" failed." << endl;
    std::cerr << "Aborting!" << endl;
    std::exit(-1);
  }

  std::vector<UnrootedTree *> unrootedTrees2  = parser.parseMultiFile(filename2); 
  if (unrootedTrees2.size() == 0 || parser.isError()) {
    std::cerr << "Error: Parsing of \"" << filename2 << "\" failed." << endl;
    std::cerr << "Aborting!" << endl;
    std::exit(-1);
  }

  return pairs_quartet_distance(unrootedTrees1, unrootedTrees2);
}

void QuartetDistanceCalculator::pairs_quartet_distance_verbose(std::ostream &out, std::vector<UnrootedTree *> &unrootedTrees1, std::vector<UnrootedTree *> &unrootedTrees2) {
  for(size_t i = 0; i < unrootedTrees1.size(); i++) {
    INTTYPE_N4 dist = calculateQuartetDistance(unrootedTrees1[i], unrootedTrees2[i]);
    
    INTTYPE_N4 resolvedQuartetsAgree = get_resolvedQuartetsAgree();
    INTTYPE_N4 resolvedQuartetsAgreeDiag = get_resolvedQuartetsAgreeDiag();
    INTTYPE_N4 resolvedQuartetsDisagree = get_resolvedQuartetsDisagree();
    INTTYPE_N4 resolvedQuartetsDisagreeDiag = get_resolvedQuartetsDisagreeDiag();
    INTTYPE_N4 resolvedQuartetsAgreeUpper = get_resolvedQuartetsAgreeUpper();
    INTTYPE_N4 resolvedQuartetsDisagreeUpper = get_resolvedQuartetsDisagreeUpper();
    
    INTTYPE_N4 n = get_n();
    INTTYPE_N4 totalNoQuartets = get_totalNoQuartets();
    double dist_norm = double(dist) / double(totalNoQuartets);
    INTTYPE_N4 resAgree = resolvedQuartetsAgree + resolvedQuartetsAgreeDiag + resolvedQuartetsAgreeUpper;
    double resAgree_norm = double(resAgree) / double(totalNoQuartets);
    INTTYPE_N4 unresolvedQuartetsAgree = get_unresolvedQuartets();
    double unresolvedQuartetsAgree_norm = double(unresolvedQuartetsAgree) / double(totalNoQuartets);
    
    std::cout << n                            << "\t"
	      << totalNoQuartets              << "\t"
	      << dist                         << "\t"
	      << dist_norm                    << "\t"
	      << resAgree                     << "\t"
	      << resAgree_norm                << "\t"
	      << unresolvedQuartetsAgree      << "\t"
	      << unresolvedQuartetsAgree_norm << std::endl;
    
  }
}

std::vector<std::vector<INTTYPE_N4> > QuartetDistanceCalculator::calculateAllPairsQuartetDistance(const char *filename) {
  NewickParser parser;

  std::vector<UnrootedTree *> unrootedTrees  = parser.parseMultiFile(filename); 
  if (unrootedTrees.size() == 0 || parser.isError()) {
    std::cerr << "Error: Parsing of \"" << filename << "\" failed." << endl;
    std::cerr << "Aborting!" << endl;
    std::exit(-1);
  }

  const std::vector<std::vector<INTTYPE_N4> > results = calculateAllPairsQuartetDistance(unrootedTrees);

  for(size_t i = 0; i < unrootedTrees.size(); ++i) {
    UnrootedTree * tmp = unrootedTrees[i];
    delete tmp;
  }

  return results;
}

std::vector<std::vector<INTTYPE_N4> > QuartetDistanceCalculator::calculateAllPairsQuartetDistance(std::vector<UnrootedTree *> trees) {
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

INTTYPE_N4 QuartetDistanceCalculator::calculateQuartetDistance(const char *filename1, const char *filename2) {
  UnrootedTree *ut1 = NULL;
  UnrootedTree *ut2 = NULL;
  NewickParser parser;

  ut1 = parser.parseFile(filename1);
  if (ut1 == NULL || parser.isError()) {
    std::cerr << "Error: Parsing of \"" << filename1 << "\" failed." << endl;
    std::cerr << "Aborting!" << endl;
    return -1;
  }

  ut2 = parser.parseFile(filename2);
  if(ut2 == NULL || parser.isError()) {
    cerr << "Parsing of file \"" << filename2 << "\" failed." << endl;
    cerr << "Aborting!" << endl;
    return -1;
  }

  INTTYPE_N4 res = calculateQuartetDistance(ut1, ut2);

  delete ut1;
  delete ut2;

  return res;
}

INTTYPE_N4 QuartetDistanceCalculator::calculateQuartetDistance(UnrootedTree *t1, UnrootedTree *t2) {

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
    std::cerr << "The two trees do not have the same set of leaves." << std::endl;
    std::cerr << "Aborting." << std::endl;
    delete this->t1->factory;
    delete this->t2->factory;
    return -1;
  }
  
  // Section 3 of Soda13: Counting unresolved triplets and quartets in a single tree
  countChildren(this->t1);
  hdt = HDT::constructHDT(this->t2, this->t1->maxDegree, dummyHDTFactory);
  
  resolvedQuartetsAgree = resolvedQuartetsAgreeDiag = 0;
  resolvedQuartetsDisagree = resolvedQuartetsDisagreeDiag = 0;
  resolvedQuartetsAgreeUpper = resolvedQuartetsDisagreeUpper = 0;
  unresolvedQuartets = 0;
  
  count(this->t1);

#ifndef doExtractAndContract
  delete hdt->factory;
#endif

  n = this->t1->n;
  totalNoQuartets = Util::binom4(n);
  INTTYPE_N4 a = resolvedQuartetsAgree + resolvedQuartetsAgreeDiag + resolvedQuartetsAgreeUpper;
  INTTYPE_N4 e = unresolvedQuartets;

  INTTYPE_N4 result = totalNoQuartets - (a + e);
  
  delete this->t1->factory;
  delete this->t2->factory;
  
  // HDT is deleted in count!
  return result;
}

void QuartetDistanceCalculator::updateCounters() {
  resolvedQuartetsAgree += hdt->quartResolvedAgree;
  resolvedQuartetsAgreeDiag += hdt->quartResolvedAgreeDiag;
  resolvedQuartetsAgreeUpper += hdt->quartResolvedAgreeUpper;
  unresolvedQuartets += hdt->quartSumE;
}
