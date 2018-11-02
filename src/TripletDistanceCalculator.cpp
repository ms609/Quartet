#include "TripletDistanceCalculator.h"

#include "hdt.h"
#include "hdt_factory.h"
#include "newick_parser.h"
#include "unrooted_tree.h"
#include "rooted_tree.h"

#include <cstdlib>
#include <vector>

TripletDistanceCalculator::TripletDistanceCalculator() {
  dummyHDTFactory = new HDTFactory(0);
}

TripletDistanceCalculator::~TripletDistanceCalculator() {
  delete dummyHDTFactory;
}

void TripletDistanceCalculator::pairs_triplet_distance_verbose(std::ostream &out, std::vector<UnrootedTree *> &unrootedTrees1, std::vector<UnrootedTree *> &unrootedTrees2) {
  RootedTree *rt1;
  RootedTree *rt2;
  
  for(size_t i = 0; i < unrootedTrees1.size(); i++) {
    
    rt1 = unrootedTrees1[i]->convertToRootedTree(NULL);
    rt2 = unrootedTrees2[i]->convertToRootedTree(rt1->factory);
    
    INTTYPE_REST dist = calculateTripletDistance(rt1, rt2);

    INTTYPE_REST n = get_n();
    INTTYPE_REST totalNoTriplets = get_totalNoTriplets();
    INTTYPE_REST resolved = get_resolvedTriplets();
    INTTYPE_REST unresolved = get_unresolvedTriplets();
    double dist_norm = double(dist) / double(totalNoTriplets);
    double resolved_norm = double(resolved) / double(totalNoTriplets);
    double unresolved_norm = double(unresolved) / double(totalNoTriplets);
    
    out << n               << "\t"
	<< totalNoTriplets << "\t"
	<< dist            << "\t"
	<< dist_norm       << "\t"
	<< resolved        << "\t"
	<< resolved_norm   << "\t"
	<< unresolved      << "\t"
	<< unresolved_norm << std::endl;
  }
}

std::vector<INTTYPE_REST> TripletDistanceCalculator::pairs_triplet_distance(const char *filename1, const char *filename2) {
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

  return pairs_triplet_distance(unrootedTrees1, unrootedTrees2);
}


std::vector<INTTYPE_REST> TripletDistanceCalculator::pairs_triplet_distance(std::vector<UnrootedTree *> &unrootedTrees1, std::vector<UnrootedTree *> &unrootedTrees2) {
  std::vector<INTTYPE_REST> res;

  RootedTree *rt1;
  RootedTree *rt2;
  
  for(size_t i = 0; i < unrootedTrees1.size(); i++) {
    rt1 = unrootedTrees1[i]->convertToRootedTree(NULL);
    rt2 = unrootedTrees2[i]->convertToRootedTree(rt1->factory);
    
    INTTYPE_REST dist = calculateTripletDistance(rt1, rt2);
    res.push_back(dist);
  }

  return res;
}

std::vector<std::vector<INTTYPE_REST> > TripletDistanceCalculator::calculateAllPairsTripletDistance(const char *filename) {
  NewickParser parser;
  
  std::vector<UnrootedTree *> unrootedTrees  = parser.parseMultiFile(filename); 
  if (unrootedTrees.size() == 0 || parser.isError()) {
    std::cerr << "Error: Parsing of \"" << filename << "\" failed." << endl;
    std::cerr << "Aborting!" << endl;
    std::exit(-1);
  }

  std::vector<std::vector<INTTYPE_REST> > results = calculateAllPairsTripletDistance(unrootedTrees);

  for(std::vector<UnrootedTree *>::iterator it = unrootedTrees.begin(); it != unrootedTrees.end(); ++it)
    delete (*it);

  return results;
}

std::vector<std::vector<INTTYPE_REST> > TripletDistanceCalculator::calculateAllPairsTripletDistance(std::vector<UnrootedTree *> trees) {
  std::vector<std::vector<INTTYPE_REST> > results(trees.size());
  
  RootedTree *rt1;
  RootedTree *rt2;

  for(size_t r = 0; r < trees.size(); ++r) {
    for(size_t c = 0; c < r; ++c) {
      rt1 = trees[r]->convertToRootedTree(NULL);
      rt2 = trees[c]->convertToRootedTree(rt1->factory);
 
      INTTYPE_REST distance = calculateTripletDistance(rt1, rt2);
      results[r].push_back(distance);

      delete rt1->factory;
      delete rt2->factory;
    }
    results[r].push_back(0);
  }

  return results;
} 
 
INTTYPE_REST TripletDistanceCalculator::calculateTripletDistance(const char *filename1, const char *filename2) {
  UnrootedTree *ut1 = NULL;
  UnrootedTree *ut2 = NULL;
  RootedTree *rt1 = NULL;
  RootedTree *rt2 = NULL;

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

  rt1 = ut1->convertToRootedTree(NULL);
  rt2 = ut2->convertToRootedTree(rt1->factory);

  INTTYPE_REST result = calculateTripletDistance(rt1, rt2);

  if (ut1 != NULL) delete ut1;
  if (ut2 != NULL) delete ut2;
  if (rt1 != NULL) delete rt1->factory;
  if (rt2 != NULL) delete rt2->factory;

  return result;
}

INTTYPE_REST TripletDistanceCalculator::calculateTripletDistance(RootedTree *t1, RootedTree *t2) {
  this->t1 = t1;
  t1->pairAltWorld(t2);
  if (t1->isError()) {
    std::cerr << "The two trees do not have the same set of leaves." << std::endl;
    std::cerr << "Aborting." << std::endl;
    return -1;
  }
  
  // Section 3 of Soda13: Counting unresolved triplets and quartets in a single tree
  countChildren(t1);

  hdt = HDT::constructHDT(t2, t1->maxDegree, dummyHDTFactory);
  
  resolvedTriplets = unresolvedTriplets = 0;
  n = t1->n;
  totalNoTriplets = Util::binom3(n);

  count(t1);
  // HDT is deleted in count if extracting and contracting!
#ifndef doExtractAndContract
  delete hdt->factory;
#endif
	
  return totalNoTriplets - resolvedTriplets - unresolvedTriplets;
}

void TripletDistanceCalculator::updateCounters() {
  resolvedTriplets += hdt->getResolvedTriplets();
  unresolvedTriplets += hdt->getUnresolvedTriplets();
}
