#ifndef QUARTET_DISTANCE_CALCULATOR_H
#define QUARTET_DISTANCE_CALCULATOR_H

#include "AbstractDistanceCalculator.h"
#include "int_stuff.h"
#include "unrooted_tree.h"
#include <Rcpp.h>

class 
QuartetDistanceCalculator : private AbstractDistanceCalculator {
 public:
  QuartetDistanceCalculator();
  ~QuartetDistanceCalculator();

  
  AE calculateQuartetAgreement(const char *t1, const char *t2);
  AE calculateQuartetAgreement(UnrootedTree *t1, UnrootedTree *t2);
  
  INTTYPE_N4 calculateQuartetDistance(const char *t1, const char *t2);
  INTTYPE_N4 calculateQuartetDistance(UnrootedTree *t1, UnrootedTree *t2);

  std::vector<std::vector<INTTYPE_N4> > calculateAllPairsQuartetDistance(const char *filename);
  std::vector<std::vector<INTTYPE_N4> > calculateAllPairsQuartetDistance(std::vector<UnrootedTree *> trees);
  
  std::vector<std::vector<std::vector<INTTYPE_N4> > > calculateAllPairsQuartetAgreement(const char *filename);
  std::vector<std::vector<std::vector<INTTYPE_N4> > > calculateAllPairsQuartetAgreement(std::vector<UnrootedTree *> trees);

  Rcpp::IntegerVector oneToManyQuartetAgreement(UnrootedTree *unrootedSingle,
    std::vector<UnrootedTree *> &unrootedMultiple);
  Rcpp::IntegerVector oneToManyQuartetAgreement(const char *fileSingle,
                                                 const char *fileMultiple);
  
  std::vector<INTTYPE_N4> pairs_quartet_distance(std::vector<UnrootedTree *> &unrootedTrees1, std::vector<UnrootedTree *> &unrootedTrees2);
  void pairs_quartet_distance_verbose(std::ostream &out, std::vector<UnrootedTree *> &unrootedTrees1, std::vector<UnrootedTree *> &unrootedTrees2);
  std::vector<INTTYPE_N4> pairs_quartet_distance(const char *filename1, const char *filename2);  

 private:
  void updateCounters();

  INTTYPE_N4 n;
  INTTYPE_N4 totalNoQuartets;
  INTTYPE_N4 resolvedQuartetsAgree, resolvedQuartetsAgreeDiag, resolvedQuartetsDisagree, resolvedQuartetsDisagreeDiag;
  INTTYPE_N4 resolvedQuartetsAgreeUpper, resolvedQuartetsDisagreeUpper;
  INTTYPE_N4 unresolvedQuartets;
};

#endif
