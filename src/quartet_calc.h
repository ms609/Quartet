#ifndef QUARTET_CALC_H
#define QUARTET_CALC_H

#include "int_stuff.h"
#include "unrooted_tree.h"

using namespace std;

typedef struct AE {
  INTTYPE_N4 a;
  INTTYPE_N4 e;
  INTTYPE_N4 noQuartets;
} AE;

class QuartetCalc {
 public:
  virtual ~QuartetCalc() {};
  virtual INTTYPE_N4 calculateQuartetDistance(UnrootedTree *t1, UnrootedTree *t2) = 0;
};
#endif
