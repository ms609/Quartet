#include "QuartetDistanceCalculator.h"
#include "test_util.hpp"

#include <cassert>

void test_quartet_distance() {
  QuartetDistanceCalculator quartetCalc;
  assert_equal(quartetCalc.calculateQuartetDistance("../trees/quartet1.new", "../trees/quartet2.new"), (INTTYPE_N4) 1);
  assert_equal(quartetCalc.calculateQuartetDistance("../trees/quartet2.new", "../trees/quartet1.new"), (INTTYPE_N4) 1);

  assert_equal(quartetCalc.calculateQuartetDistance("../trees/test_tree1.new", "../trees/test_tree2.new"), (INTTYPE_N4) 26);
  assert_equal(quartetCalc.calculateQuartetDistance("../trees/test_tree2.new", "../trees/test_tree1.new"), (INTTYPE_N4) 26);

  assert_equal(quartetCalc.calculateQuartetDistance("../trees/test_tree3.new", "../trees/test_tree4.new"), (INTTYPE_N4) 5485860);
  assert_equal(quartetCalc.calculateQuartetDistance("../trees/test_tree4.new", "../trees/test_tree3.new"), (INTTYPE_N4) 5485860);
}

void test_all_pairs_quartet_distance() {
  QuartetDistanceCalculator quartetCalc;
  
  const std::vector<std::vector<INTTYPE_N4> > &result = quartetCalc.calculateAllPairsQuartetDistance("../trees/five_trees.new");
  
  assert_equal(result[1][0], (INTTYPE_N4) 1);
  assert_equal(result[0][0], (INTTYPE_N4) 0);
  assert_equal(result[2][1], (INTTYPE_N4) 1);
  assert_equal(result[3][2], (INTTYPE_N4) 1);
}

int main(int argc, char **argv) {
  test_quartet_distance();
  test_all_pairs_quartet_distance();

  return 0;
}
