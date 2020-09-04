#include <Rcpp.h>
using namespace Rcpp;
typedef int_fast16_t int16;
typedef int_fast32_t int32;

// [[Rcpp::export]]
IntegerMatrix all_quartets(IntegerVector nTips) {
  int16 n = nTips[0];
  int32 n_quartets = n * (n - 1) * (n - 2) * (n - 3) / (1 * 2 * 3 * 4);
  int32 q = n_quartets - 1;
  IntegerMatrix ret(4, n_quartets);
  
  for (int16 i = n - 3; i != 0; i--)
  for (int16 j = n - 2; j != i; j--)
  for (int16 k = n - 1; k != j; k--)
  for (int16 l = n - 0; l != k; l--) {
    ret(0, q) = i;
    ret(1, q) = j;
    ret(2, q) = k;
    ret(3, q) = l;
    q--;
  }
  return ret;
}
