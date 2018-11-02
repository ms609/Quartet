#ifndef ABSTRACT_DISTANCE_CALCULATOR_H
#define ABSTRACT_DISTANCE_CALCULATOR_H

#include "int_stuff.h"
#include "triplet_calc.h"
#include "quartet_calc.h"
#include "hdt.h"

#ifndef CONTRACT_MAX_EXTRA_SIZE
#define CONTRACT_MAX_EXTRA_SIZE 20000
#endif

class AbstractDistanceCalculator {
 public:
  virtual ~AbstractDistanceCalculator() { }

 protected:
  void countChildren(RootedTree *t);
  void count(RootedTree *v);
  virtual void updateCounters() = 0;

  HDTFactory *dummyHDTFactory;
  RootedTree *t1, *t2;
  HDT *hdt;
};

#endif
