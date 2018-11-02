#ifndef TRIPLET_DISTANCE_CALCULATOR_H
#define TRIPLET_DISTANCE_CALCULATOR_H

#include "AbstractDistanceCalculator.h"
#include "int_stuff.h"
#include "rooted_tree.h"

#include <string>
#include <vector>

class 
#ifdef _WIN32
	__declspec(dllexport)
#endif
TripletDistanceCalculator : private AbstractDistanceCalculator {
 public:
  TripletDistanceCalculator();
  ~TripletDistanceCalculator();

  INTTYPE_REST calculateTripletDistance(const char *filename1, const char *filename2);
  INTTYPE_REST calculateTripletDistance(RootedTree *t1, RootedTree *t2);

  std::vector<std::vector<INTTYPE_REST> > calculateAllPairsTripletDistance(const char *filename);
  std::vector<std::vector<INTTYPE_REST> > calculateAllPairsTripletDistance(std::vector<UnrootedTree *> trees);

  void pairs_triplet_distance_verbose(std::ostream &out, std::vector<UnrootedTree *> &unrootedTrees1, std::vector<UnrootedTree *> &unrootedTrees2);
  std::vector<INTTYPE_REST> pairs_triplet_distance(std::vector<UnrootedTree *> &unrootedTrees1, std::vector<UnrootedTree *> &unrootedTrees2);
  std::vector<INTTYPE_REST> pairs_triplet_distance(const char *filename1, const char *filename2);  

  // accessors
  INTTYPE_REST get_n() { return n; }
  INTTYPE_REST get_resolvedTriplets() { return resolvedTriplets; }
  INTTYPE_REST get_unresolvedTriplets() { return unresolvedTriplets; }
  INTTYPE_REST get_totalNoTriplets() { return totalNoTriplets; }

 private:
  void updateCounters();

  INTTYPE_REST n;
  INTTYPE_REST totalNoTriplets;
  INTTYPE_REST resolvedTriplets, unresolvedTriplets;
};

#endif
