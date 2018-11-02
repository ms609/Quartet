#ifndef QUARTET_DISTANCE_CALCULATOR_H
#define QUARTET_DISTANCE_CALCULATOR_H

#include "AbstractDistanceCalculator.h"
#include "int_stuff.h"
#include "unrooted_tree.h"

class 
#ifdef _WIN32
	__declspec(dllexport)
#endif
QuartetDistanceCalculator : private AbstractDistanceCalculator {
 public:
  QuartetDistanceCalculator();
  ~QuartetDistanceCalculator();

  INTTYPE_N4 calculateQuartetDistance(const char *t1, const char *t2);
  INTTYPE_N4 calculateQuartetDistance(UnrootedTree *t1, UnrootedTree *t2);

  std::vector<std::vector<INTTYPE_N4> > calculateAllPairsQuartetDistance(const char *filename);
  std::vector<std::vector<INTTYPE_N4> > calculateAllPairsQuartetDistance(std::vector<UnrootedTree *> trees);

  std::vector<INTTYPE_N4> pairs_quartet_distance(std::vector<UnrootedTree *> &unrootedTrees1, std::vector<UnrootedTree *> &unrootedTrees2);
  void pairs_quartet_distance_verbose(std::ostream &out, std::vector<UnrootedTree *> &unrootedTrees1, std::vector<UnrootedTree *> &unrootedTrees2);
  std::vector<INTTYPE_N4> pairs_quartet_distance(const char *filename1, const char *filename2);  


  // accessors
  INTTYPE_N4 get_n() { return n; }
  INTTYPE_N4 get_totalNoQuartets() { return totalNoQuartets; }
  INTTYPE_N4 get_resolvedQuartetsAgree() { return resolvedQuartetsAgree; }
  INTTYPE_N4 get_resolvedQuartetsAgreeDiag() { return resolvedQuartetsAgreeDiag; }
  INTTYPE_N4 get_resolvedQuartetsDisagree() { return resolvedQuartetsDisagree; }
  INTTYPE_N4 get_resolvedQuartetsDisagreeDiag() { return resolvedQuartetsDisagreeDiag; }
  INTTYPE_N4 get_resolvedQuartetsAgreeUpper() { return resolvedQuartetsAgreeUpper; }
  INTTYPE_N4 get_resolvedQuartetsDisagreeUpper() { return resolvedQuartetsDisagreeUpper; }
  INTTYPE_N4 get_unresolvedQuartets() { return unresolvedQuartets; }
  
 private:
  void updateCounters();

  INTTYPE_N4 n;
  INTTYPE_N4 totalNoQuartets;
  INTTYPE_N4 resolvedQuartetsAgree, resolvedQuartetsAgreeDiag, resolvedQuartetsDisagree, resolvedQuartetsDisagreeDiag;
  INTTYPE_N4 resolvedQuartetsAgreeUpper, resolvedQuartetsDisagreeUpper;
  INTTYPE_N4 unresolvedQuartets;
};

#endif
