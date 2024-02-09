#include <Rcpp.h>
#include <cstdint> // for INT_FAST16_MAX
using namespace Rcpp;
typedef int_fast16_t int16;
typedef int_fast32_t int32;

const int16 QD_MAX_TIPS = 100;
const int16 SPLIT_CHUNK = 8;

int32 tri_num[QD_MAX_TIPS + 1];
int32 tet_num[QD_MAX_TIPS + 1];
int32 hyp_num[QD_MAX_TIPS + 1];

__attribute__((constructor)) // Construction avoids floating point worries
  void initialize_triangles() {
    tri_num[0] = 0;
    tet_num[0] = 0;
    hyp_num[0] = 0;
    for (int16 i = 0; i != QD_MAX_TIPS; ++i) {
      const int16 nxt = i + 1;
      tri_num[nxt] = tri_num[i] + nxt;
      tet_num[nxt] = tet_num[i] + tri_num[nxt];
      hyp_num[nxt] = hyp_num[i] + tet_num[nxt];
    }
  }



// [[Rcpp::export]]
IntegerMatrix all_quartets(IntegerVector nTips) {
  if (!nTips.length()) {
    Rcpp::stop("nTips must contain a single integer value");
  }
  if (INT_FAST16_MAX < INT_MAX && // Supress warning of tautological comparison
      nTips[0] >= int32(INT_FAST16_MAX)) {
    Rcpp::stop("nTips must be < 32768");
  }
  if (nTips[0] < 4) {
    Rcpp::stop("nTips must be at least 4");
  }
  
  int16 n = nTips[0];
  if (int32(n) != nTips[0]) {
    Rcpp::stop("Integer overflow: nTips must be < 32768. Contact maintainer.");
  }
  if (int32(n) > 55108) {
    // n * (n - 1) * (n - 2) * (n - 3) > INT_FAST32_MAX
    Rcpp::stop("int32 overflow: nTips must be < 55108.");
  }
  
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


// [[Rcpp::export]]
int which_index(IntegerVector indices, IntegerVector m) {
  if (indices.length() != 4) {
    Rcpp::stop("4 indices needed");
  }
  if (m[0] > int(QD_MAX_TIPS)) {
    Rcpp::stop("Too many tips for which_index()");
  }
  const int16
    n_tips = m[0],
    a = indices[0],
    b = indices[1],
    c = indices[2],
    d = indices[3],
    choices1 = n_tips - 3,
    choices2 = n_tips - a - 3,
    choices3 = n_tips - b - 2,
    chosen1 = a,
    chosen2 = b - a - 1,
    chosen3 = c - b - 1,
    chosen4 = d - c - 1
  ;
  if (a < 0) {
    Rcpp::stop("indices[0] must be positive");
  }
  if (d >= n_tips) {
    Rcpp::stop("indices[3] must be less than m");
  }
  if (a < b && b < c && c < d) {
    return (hyp_num[choices1] - hyp_num[choices1 - chosen1])
         + (tet_num[choices2] - tet_num[choices2 - chosen2])
         + (tri_num[choices3] - tri_num[choices3 - chosen3])
         + chosen4;
  } else {
    Rcpp::stop("a < b < c < d not satisfied");
  }
}

int32 n_quartets(int16 n_tips) {
  return hyp_num[n_tips - 3];
}

// [[Rcpp::export]]
RawVector quartet_states(RawMatrix splits) {
  const int16 n_tip = splits.attr("nTip");
  if (n_tip > QD_MAX_TIPS) {
    Rcpp::stop("Too many leaves for quartet_states()");
  }
  if (n_tip < 4) {
    Rcpp::stop("Need four leaves to define quartets");
  }
  
  const unsigned char bitmask[8] = {1U, 2U, 4U, 8U, 16U, 32U, 64U, 128U};
  RawVector ret(n_quartets(n_tip));
  int32 q = 0;
  for (int16 a = 0; a != n_tip - 3; a++) {
    const int16 
      a_mask = bitmask[a % SPLIT_CHUNK],
      a_chunk = a / SPLIT_CHUNK
    ;
    for (int16 b = a + 1; b != n_tip - 2; b++) {
      const int16 
        b_mask = bitmask[b % SPLIT_CHUNK],
        b_chunk = b / SPLIT_CHUNK
      ;
      for (int16 c = b + 1; c != n_tip - 1; c++) {
        const int16 
          c_mask = bitmask[c % SPLIT_CHUNK],
          c_chunk = c / SPLIT_CHUNK
        ;
        for (int16 d = c + 1; d != n_tip; d++) {
          const int16 
            d_mask = bitmask[d % SPLIT_CHUNK],
            d_chunk = d / SPLIT_CHUNK
          ;
          for (int16 split = 0; split != splits.nrow(); split++) {
            const bool 
              a_state = (unsigned char) (splits(split, a_chunk)) & a_mask,
              b_state = (unsigned char) (splits(split, b_chunk)) & b_mask,
              c_state = (unsigned char) (splits(split, c_chunk)) & c_mask,
              d_state = (unsigned char) (splits(split, d_chunk)) & d_mask
            ;
            if (a_state) {
              if (b_state) {
                if (!c_state & !d_state) {
                  ret[q] = 3;
                  break;
                }
              } else { // a & !b
                if (c_state) {
                  if (!d_state) {
                    ret[q] = 2;
                    break;
                  }
                } else { // a & !b & !c
                  if (d_state) {
                    ret[q] = 1;
                    break;
                  }
                }
              }
            } else { // !a
              if (b_state) { // !a & b
                if (c_state) {
                  if (!d_state) {
                    ret[q] = 1;
                    break;
                  }
                } else if (d_state) {
                  ret[q] = 2;
                  break;
                }
              } else { // !a, !b
                if (c_state & d_state) { // !a, !b, c, d
                  ret[q] = 3;
                  break;
                }
              }
            }
          }
          q++;
        }
      }
    }
  }
  return ret;
}
