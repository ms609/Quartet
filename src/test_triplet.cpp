#include "TripletDistanceCalculator.h"
#include "test_util.hpp"

#include <cassert>

void test_triplet_distance() {
  TripletDistanceCalculator tripletCalc;
  assert_equal(tripletCalc.calculateTripletDistance("../trees/tree_ab-c.new", "../trees/tree_ac-b.new"), (INTTYPE_REST) 1);
  assert_equal(tripletCalc.calculateTripletDistance("../trees/tree_ac-b.new", "../trees/tree_ab-c.new"), (INTTYPE_REST) 1);

  assert_equal(tripletCalc.calculateTripletDistance("../trees/test_tree1.new", "../trees/test_tree2.new"), (INTTYPE_REST) 26);
  assert_equal(tripletCalc.calculateTripletDistance("../trees/test_tree2.new", "../trees/test_tree1.new"), (INTTYPE_REST) 26);

  assert_equal(tripletCalc.calculateTripletDistance("../trees/test_tree3.new", "../trees/test_tree4.new"), (INTTYPE_REST) 187793);
  assert_equal(tripletCalc.calculateTripletDistance("../trees/test_tree4.new", "../trees/test_tree3.new"), (INTTYPE_REST) 187793);
}

void test_all_pairs_triplet_distance() {
  TripletDistanceCalculator tripletCalc;
  
  const std::vector<std::vector<INTTYPE_REST> > &result = tripletCalc.calculateAllPairsTripletDistance("../trees/two_trees.new");
  
  assert_equal(result[1][0], (INTTYPE_REST) 26);
  assert_equal(result[0][0], (INTTYPE_REST) 0);
  assert_equal(result[2][1], (INTTYPE_REST) 26);
}


int main(int argc, char **argv) {
  test_triplet_distance();
  test_all_pairs_triplet_distance();

  return 0;
}
