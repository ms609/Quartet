#ifndef TRIPLET_CALC_H
#define TRIPLET_CALC_H

#include "int_stuff.h"
#include "rooted_tree.h"

using namespace std;

class TripletCalc {
 public:
  virtual ~TripletCalc() {};
  virtual INTTYPE_REST calculateTripletDistance(RootedTree *t1, RootedTree *t2) = 0;
};
#endif
