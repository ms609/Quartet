/* QuartetConsensus.cpp
 *
 * C++ core for the QuartetConsensus algorithm (Takazawa et al. 2026).
 *
 * Finds the tree minimizing the sum of symmetric quartet distances
 * to a set of input trees via a greedy add-and-prune heuristic.
 *
 * The symmetric quartet distance is: 2d + r1 + r2
 * (using the Estabrook et al. categories from QuartetStatus).
 */

#include <Rcpp.h>
#include <algorithm>
#include <cstdint>
#include <numeric>
#include <unordered_map>
#include <vector>

using Rcpp::IntegerVector;
using Rcpp::List;
using Rcpp::LogicalVector;
using Rcpp::RawMatrix;
using Rcpp::RawVector;

typedef int_fast16_t int16;
typedef int_fast32_t int32;

// Reuse combinatorial lookup tables from AllQuartets.cpp
extern int32 tri_num[];
extern int32 tet_num[];
extern int32 hyp_num[];

static const int16 QC_MAX_TIPS = 100;
static const int16 SPLIT_CHUNK = 8;


// ============================================================================
// Quartet index computation (reuses AllQuartets.cpp formula)
// ============================================================================

// Given 0-based indices a < b < c < d, return the quartet index.
inline int32 quartet_index(int16 a, int16 b, int16 c, int16 d,
                           int16 n_tips) {
  const int16
    choices1 = n_tips - 3,
    choices2 = n_tips - a - 3,
    choices3 = n_tips - b - 2,
    chosen1  = a,
    chosen2  = b - a - 1,
    chosen3  = c - b - 1,
    chosen4  = d - c - 1;
  return (hyp_num[choices1] - hyp_num[choices1 - chosen1])
       + (tet_num[choices2] - tet_num[choices2 - chosen2])
       + (tri_num[choices3] - tri_num[choices3 - chosen3])
       + chosen4;
}

// Defined in AllQuartets.cpp
extern int32 n_quartets(int16 n_tips);


// ============================================================================
// Quartet state from a single split for a single quartet
// ============================================================================

// For quartet {i,j,k,l} (i < j < k < l), determine the quartet state
// (0 = unresolved, 1 = il|jk, 2 = jl|ik, 3 = kl|ij) given which side
// of the split each tip is on.
//
// The state encoding matches AllQuartets.cpp:
// - State 1: i and l on same side (ad|bc pattern)
// - State 2: j and l on same side (bd|ac pattern)
// - State 3: k and l on same side (cd|ab pattern)
//
// Returns 0 if the split doesn't have exactly 2 on each side.
inline int quartet_state_from_sides(bool si, bool sj, bool sk, bool sl) {
  int sum = si + sj + sk + sl;
  if (sum != 2) return 0;
  if (si == sl) return 1;  // i,l same side → il|jk
  if (sj == sl) return 2;  // j,l same side → jl|ik
  // sk == sl must be true  → kl|ij
  return 3;
}


// ============================================================================
// Pooled split representation
// ============================================================================

struct PooledSplits {
  int n_splits;
  int n_bytes;   // number of bytes per split in raw representation
  int n_tips;

  // Raw split data: n_splits * n_bytes elements.
  // Split i occupies [i*n_bytes .. (i+1)*n_bytes).
  std::vector<unsigned char> data;

  // Per-split metadata
  std::vector<int> count;       // how many trees contain this split
  std::vector<int> light_side;  // min(popcount, n_tips - popcount)

  // Per-split: list of tip indices on the "1" side (canonical)
  std::vector<std::vector<int16>> tips_on_side1;

  // Tree membership
  std::vector<std::vector<int>> tree_members;

  const unsigned char* split(int i) const {
    return &data[i * n_bytes];
  }
  unsigned char* split(int i) {
    return &data[i * n_bytes];
  }
};


// ============================================================================
// FNV-1a hash for canonical split arrays
// ============================================================================

struct SplitHash {
  int n_bytes;
  explicit SplitHash(int nb) : n_bytes(nb) {}
  SplitHash() : n_bytes(0) {}

  std::size_t operator()(const unsigned char* sp) const {
    std::size_t h = 14695981039346656037ULL;
    for (int i = 0; i < n_bytes; ++i) {
      h ^= static_cast<std::size_t>(sp[i]);
      h *= 1099511628211ULL;
    }
    return h;
  }
};

struct SplitEqual {
  int n_bytes;
  explicit SplitEqual(int nb) : n_bytes(nb) {}
  SplitEqual() : n_bytes(0) {}

  bool operator()(const unsigned char* a, const unsigned char* b) const {
    for (int i = 0; i < n_bytes; ++i) {
      if (a[i] != b[i]) return false;
    }
    return true;
  }
};


// ============================================================================
// pool_splits: deduplicate and canonicalise all splits from all trees
// ============================================================================

static PooledSplits pool_splits(const List& splits_list, int n_tips) {
  const int n_tree = splits_list.size();
  const unsigned char bitmask[8] = {1U, 2U, 4U, 8U, 16U, 32U, 64U, 128U};

  const RawMatrix first_mat = Rcpp::as<RawMatrix>(splits_list[0]);
  const int n_bytes = first_mat.ncol();

  // Mask for the last byte
  const int used_bits = ((n_tips - 1) % 8) + 1;
  const unsigned char last_mask =
    static_cast<unsigned char>((1U << used_bits) - 1U);

  SplitHash hasher(n_bytes);
  SplitEqual eq(n_bytes);
  std::unordered_map<const unsigned char*, int, SplitHash, SplitEqual>
    split_map(64, hasher, eq);

  if (n_bytes < 1) {
    Rcpp::stop("Internal error: n_bytes < 1 in pool_splits (n_tips = %d).",
               n_tips);
  }
  std::vector<unsigned char> canon_buf(n_bytes);

  PooledSplits pool;
  pool.n_tips = n_tips;
  pool.n_bytes = n_bytes;
  pool.n_splits = 0;
  pool.tree_members.resize(n_tree);

  // Reserve pool.data so it never reallocates.  split_map stores raw pointers
  // into this buffer, so reallocation would create dangling keys.
  size_t total_splits = 0;
  for (int t = 0; t < n_tree; ++t) {
    const RawMatrix mat_t = Rcpp::as<RawMatrix>(splits_list[t]);
    total_splits += mat_t.nrow();
  }
  pool.data.reserve(total_splits * n_bytes);

  for (int t = 0; t < n_tree; ++t) {
    const RawMatrix mat = Rcpp::as<RawMatrix>(splits_list[t]);
    const int n_sp = mat.nrow();
    std::vector<int>& members = pool.tree_members[t];
    members.reserve(n_sp);

    for (int s = 0; s < n_sp; ++s) {
      // Copy raw bytes
      for (int b = 0; b < n_bytes; ++b) {
        canon_buf[b] = static_cast<unsigned char>(mat(s, b));
      }
      canon_buf[n_bytes - 1] &= last_mask;

      // Canonicalise: if bit 0 is set, flip
      if (canon_buf[0] & 1U) {
        for (int b = 0; b < n_bytes; ++b) {
          canon_buf[b] = ~canon_buf[b];
        }
        canon_buf[n_bytes - 1] &= last_mask;
      }

      auto it = split_map.find(canon_buf.data());
      int idx;
      if (it != split_map.end()) {
        idx = it->second;
        pool.count[idx]++;
      } else {
        idx = pool.n_splits++;
        const size_t old_sz = pool.data.size();
        pool.data.resize(old_sz + n_bytes);
        std::copy(canon_buf.begin(), canon_buf.end(),
                  pool.data.begin() + old_sz);
        // Popcount
        int pc = 0;
        for (int b = 0; b < n_bytes; ++b) {
          unsigned char byte = canon_buf[b];
          while (byte) { pc += byte & 1; byte >>= 1; }
        }
        pool.count.push_back(1);
        pool.light_side.push_back(std::min(pc, n_tips - pc));

        // Build tip list for side 1 (canonical side: bit 0 is OFF)
        std::vector<int16> tips1;
        for (int16 tip = 0; tip < n_tips; ++tip) {
          if (canon_buf[tip / 8] & bitmask[tip % 8]) {
            tips1.push_back(tip);
          }
        }
        pool.tips_on_side1.push_back(std::move(tips1));

        split_map[pool.split(idx)] = idx;
      }

      // Record unique membership per tree
      bool found = false;
      for (int m : members) {
        if (m == idx) { found = true; break; }
      }
      if (!found) members.push_back(idx);
    }
  }

  return pool;
}


// ============================================================================
// build_quartet_profile: for each quartet, count how many input trees
// resolve it as each of the 3 states (or leave unresolved).
// ============================================================================

// Returns a flat array of 4 * n_q ints:
// profile[q * 4 + s] = count of trees with state s for quartet q.
static std::vector<int> build_quartet_profile(
    const List& splits_list,
    int n_tips
) {
  const unsigned char bitmask[8] = {1U, 2U, 4U, 8U, 16U, 32U, 64U, 128U};
  const int32 n_q = n_quartets(static_cast<int16>(n_tips));
  const int n_tree = splits_list.size();

  std::vector<int> profile(n_q * 4, 0);

  for (int t = 0; t < n_tree; ++t) {
    Rcpp::checkUserInterrupt();
    const RawMatrix splits = Rcpp::as<RawMatrix>(splits_list[t]);
    const int n_sp = splits.nrow();

    // Compute quartet states for this tree (same as quartet_states())
    int32 q = 0;
    for (int16 a = 0; a < n_tips - 3; ++a) {
      const int16 a_mask = bitmask[a % SPLIT_CHUNK];
      const int16 a_chunk = a / SPLIT_CHUNK;
      for (int16 b = a + 1; b < n_tips - 2; ++b) {
        const int16 b_mask = bitmask[b % SPLIT_CHUNK];
        const int16 b_chunk = b / SPLIT_CHUNK;
        for (int16 c = b + 1; c < n_tips - 1; ++c) {
          const int16 c_mask = bitmask[c % SPLIT_CHUNK];
          const int16 c_chunk = c / SPLIT_CHUNK;
          for (int16 d = c + 1; d < n_tips; ++d) {
            const int16 d_mask = bitmask[d % SPLIT_CHUNK];
            const int16 d_chunk = d / SPLIT_CHUNK;
            int state = 0;
            for (int sp = 0; sp < n_sp; ++sp) {
              const bool
                a_state = static_cast<unsigned char>(splits(sp, a_chunk))
                          & a_mask,
                b_state = static_cast<unsigned char>(splits(sp, b_chunk))
                          & b_mask,
                c_state = static_cast<unsigned char>(splits(sp, c_chunk))
                          & c_mask,
                d_state = static_cast<unsigned char>(splits(sp, d_chunk))
                          & d_mask;
              if (a_state) {
                if (b_state) {
                  if (!c_state && !d_state) { state = 3; break; }
                } else {
                  if (c_state) {
                    if (!d_state) { state = 2; break; }
                  } else {
                    if (d_state) { state = 1; break; }
                  }
                }
              } else {
                if (b_state) {
                  if (c_state) {
                    if (!d_state) { state = 1; break; }
                  } else if (d_state) { state = 2; break; }
                } else {
                  if (c_state && d_state) { state = 3; break; }
                }
              }
            }
            profile[q * 4 + state]++;
            q++;
          }
        }
      }
    }
  }

  return profile;
}


// ============================================================================
// compat_mat: pairwise compatibility between pooled splits
// ============================================================================

static std::vector<uint8_t> compat_mat(const PooledSplits& pool) {
  const int M = pool.n_splits;
  const int nb = pool.n_bytes;
  const int n_tips = pool.n_tips;
  const int used_bits = ((n_tips - 1) % 8) + 1;
  const unsigned char last_mask =
    static_cast<unsigned char>((1U << used_bits) - 1U);

  std::vector<uint8_t> compat(M * M, 1);

  for (int i = 0; i < M; ++i) {
    const unsigned char* a = pool.split(i);
    for (int j = i + 1; j < M; ++j) {
      const unsigned char* b = pool.split(j);
      bool ab = false, anb = false, nab = false, nanb = false;
      for (int byte_idx = 0; byte_idx < nb; ++byte_idx) {
        unsigned char mask = (byte_idx == nb - 1) ? last_mask : 0xFF;
        unsigned char a_bin = a[byte_idx] & mask;
        unsigned char b_bin = b[byte_idx] & mask;
        if (!ab)   ab   = (a_bin & b_bin) != 0;
        if (!anb)  anb  = (a_bin & ~b_bin & mask) != 0;
        if (!nab)  nab  = (~a_bin & b_bin & mask) != 0;
        if (!nanb) nanb = (~a_bin & ~b_bin & mask) != 0;
        if (ab && anb && nab && nanb) break;
      }
      bool comp = !ab || !anb || !nab || !nanb;
      compat[i * M + j] = comp ? 1 : 0;
      compat[j * M + i] = comp ? 1 : 0;
    }
  }
  return compat;
}


// ============================================================================
// Greedy state for quartet consensus
// ============================================================================

struct QCGreedyState {
  int M;            // number of pooled splits
  int n_tips;
  int n_q;          // C(n, 4)
  int k;            // number of input trees
  int n_incl;       // currently included splits

  std::vector<uint8_t>  incl;            // which splits are included
  std::vector<int>&     profile;         // quartet profile [n_q * 4]
  const PooledSplits&   pool;
  const std::vector<uint8_t>& compat;

  // Per-quartet state in the current consensus
  std::vector<int>  consensus_state;     // 0 = unresolved, 1-3 = resolved
  std::vector<int>  resolve_count;       // how many included splits resolve it

  // Cached per-split: incompatibility counts with included splits
  std::vector<int>  n_incompat;

  // Current total loss
  double total_loss;

  QCGreedyState(
      std::vector<int>& profile_,
      const PooledSplits& pool_,
      const std::vector<uint8_t>& compat_,
      int M_, int n_tips_, int k_
  ) : M(M_), n_tips(n_tips_),
      n_q(n_quartets(static_cast<int16>(n_tips_))),
      k(k_), n_incl(0),
      incl(M_, 0), profile(profile_), pool(pool_), compat(compat_),
      consensus_state(n_q, 0), resolve_count(n_q, 0),
      n_incompat(M_, 0), total_loss(0.0)
  {
    // Initial loss: all quartets unresolved in consensus
    // cost(q) = k - count_0[q]
    for (int q = 0; q < n_q; ++q) {
      total_loss += k - profile[q * 4 + 0];
    }
  }

  bool is_compatible(int idx) const {
    return n_incompat[idx] == 0 && n_incl < n_tips - 3;
  }

  // Compute benefit of adding split c.
  // For each quartet newly resolved by c (currently unresolved in consensus):
  //   delta = k - 2 * count_j[q]
  //   benefit = -delta (we want to minimize loss)
  double add_benefit(int c) const {
    const unsigned char bitmask[8] = {1U, 2U, 4U, 8U, 16U, 32U, 64U, 128U};
    const unsigned char* sp = pool.split(c);
    const auto& tips1 = pool.tips_on_side1[c];
    const int m1 = static_cast<int>(tips1.size());
    const int m0 = n_tips - m1;

    // Build side-0 tip list
    std::vector<int16> tips0;
    tips0.reserve(m0);
    {
      int idx1 = 0;
      for (int16 tip = 0; tip < n_tips; ++tip) {
        if (idx1 < m1 && tips1[idx1] == tip) {
          idx1++;
        } else {
          tips0.push_back(tip);
        }
      }
    }

    double benefit = 0.0;

    // Enumerate quartets with 2 tips from each side
    for (int ai = 0; ai < m0 - 1; ++ai) {
      for (int bi = ai + 1; bi < m0; ++bi) {
        for (int ci = 0; ci < m1 - 1; ++ci) {
          for (int di = ci + 1; di < m1; ++di) {
            // 4 tips: tips0[ai], tips0[bi], tips1[ci], tips1[di]
            // Sort them
            int16 t[4] = {tips0[ai], tips0[bi], tips1[ci], tips1[di]};
            // Insertion sort for 4 elements
            for (int x = 1; x < 4; ++x) {
              int16 key = t[x];
              int y = x - 1;
              while (y >= 0 && t[y] > key) {
                t[y + 1] = t[y];
                y--;
              }
              t[y + 1] = key;
            }

            int32 qi = quartet_index(t[0], t[1], t[2], t[3],
                                     static_cast<int16>(n_tips));

            // Only count if currently unresolved in consensus
            if (resolve_count[qi] > 0) continue;

            // Determine the state this split gives the quartet
            bool sa = sp[t[0] / 8] & bitmask[t[0] % 8];
            bool sb = sp[t[1] / 8] & bitmask[t[1] % 8];
            bool sc = sp[t[2] / 8] & bitmask[t[2] % 8];
            bool sd = sp[t[3] / 8] & bitmask[t[3] % 8];
            int state = quartet_state_from_sides(sa, sb, sc, sd);
            if (state == 0) continue;  // shouldn't happen for 2+2 split

            // benefit = (old cost) - (new cost)
            // old cost (unresolved) = k - count_0[q]
            // new cost (resolved as j) = 2(k - count_0[q] - count_j[q]) + count_0[q]
            // delta = new - old = k - 2*count_j[q]
            // benefit = -delta = 2*count_j[q] - k
            int count_j = profile[qi * 4 + state];
            benefit += 2.0 * count_j - k;
          }
        }
      }
    }
    return benefit;
  }

  // Compute benefit of removing split c.
  // For each quartet uniquely resolved by c (resolve_count == 1):
  //   benefit = k - 2*count_j[q]
  double remove_benefit(int c) const {
    const auto& tips1 = pool.tips_on_side1[c];
    const int m1 = static_cast<int>(tips1.size());

    std::vector<int16> tips0;
    tips0.reserve(n_tips - m1);
    {
      int idx1 = 0;
      for (int16 tip = 0; tip < n_tips; ++tip) {
        if (idx1 < m1 && tips1[idx1] == tip) {
          idx1++;
        } else {
          tips0.push_back(tip);
        }
      }
    }
    const int m0 = static_cast<int>(tips0.size());

    double benefit = 0.0;

    for (int ai = 0; ai < m0 - 1; ++ai) {
      for (int bi = ai + 1; bi < m0; ++bi) {
        for (int ci = 0; ci < m1 - 1; ++ci) {
          for (int di = ci + 1; di < m1; ++di) {
            int16 t[4] = {tips0[ai], tips0[bi], tips1[ci], tips1[di]};
            for (int x = 1; x < 4; ++x) {
              int16 key = t[x];
              int y = x - 1;
              while (y >= 0 && t[y] > key) {
                t[y + 1] = t[y];
                y--;
              }
              t[y + 1] = key;
            }

            int32 qi = quartet_index(t[0], t[1], t[2], t[3],
                                     static_cast<int16>(n_tips));

            // Only count if uniquely resolved by this split
            if (resolve_count[qi] != 1) continue;
            // Double-check it's actually resolved
            if (consensus_state[qi] == 0) continue;

            int state = consensus_state[qi];
            int count_j = profile[qi * 4 + state];
            // benefit of removing: old cost - new cost
            // old cost = 2(k - count_0[q] - count_j[q]) + count_0[q]
            // new cost = k - count_0[q]
            // delta = new - old = -(k - 2*count_j[q])
            // benefit = -delta = k - 2*count_j[q]
            benefit += k - 2.0 * count_j;
          }
        }
      }
    }
    return benefit;
  }

  // Execute: add split c to the consensus
  void do_add(int c) {
    const unsigned char bitmask[8] = {1U, 2U, 4U, 8U, 16U, 32U, 64U, 128U};
    incl[c] = 1;
    n_incl++;

    // Update n_incompat
    for (int j = 0; j < M; ++j) {
      if (!compat[j * M + c]) n_incompat[j]++;
    }

    // Update quartet states
    const unsigned char* sp = pool.split(c);
    const auto& tips1 = pool.tips_on_side1[c];
    const int m1 = static_cast<int>(tips1.size());

    std::vector<int16> tips0;
    tips0.reserve(n_tips - m1);
    {
      int idx1 = 0;
      for (int16 tip = 0; tip < n_tips; ++tip) {
        if (idx1 < m1 && tips1[idx1] == tip) {
          idx1++;
        } else {
          tips0.push_back(tip);
        }
      }
    }
    const int m0 = static_cast<int>(tips0.size());

    for (int ai = 0; ai < m0 - 1; ++ai) {
      for (int bi = ai + 1; bi < m0; ++bi) {
        for (int ci = 0; ci < m1 - 1; ++ci) {
          for (int di = ci + 1; di < m1; ++di) {
            int16 t[4] = {tips0[ai], tips0[bi], tips1[ci], tips1[di]};
            for (int x = 1; x < 4; ++x) {
              int16 key = t[x];
              int y = x - 1;
              while (y >= 0 && t[y] > key) {
                t[y + 1] = t[y];
                y--;
              }
              t[y + 1] = key;
            }

            int32 qi = quartet_index(t[0], t[1], t[2], t[3],
                                     static_cast<int16>(n_tips));

            bool sa = sp[t[0] / 8] & bitmask[t[0] % 8];
            bool sb = sp[t[1] / 8] & bitmask[t[1] % 8];
            bool sc = sp[t[2] / 8] & bitmask[t[2] % 8];
            bool sd = sp[t[3] / 8] & bitmask[t[3] % 8];
            int state = quartet_state_from_sides(sa, sb, sc, sd);
            if (state == 0) continue;

            resolve_count[qi]++;
            if (resolve_count[qi] == 1) {
              // Quartet just became resolved
              consensus_state[qi] = state;
              int count_j = profile[qi * 4 + state];
              // delta_loss = (2(k - count_0 - count_j) + count_0) - (k - count_0)
              //            = k - 2*count_j
              total_loss += k - 2.0 * count_j;
            }
          }
        }
      }
    }
  }

  // Execute: remove split c from the consensus
  void do_remove(int c) {
    const unsigned char bitmask[8] = {1U, 2U, 4U, 8U, 16U, 32U, 64U, 128U};
    incl[c] = 0;
    n_incl--;

    // Update n_incompat
    for (int j = 0; j < M; ++j) {
      if (!compat[j * M + c]) n_incompat[j]--;
    }

    const unsigned char* sp = pool.split(c);
    const auto& tips1 = pool.tips_on_side1[c];
    const int m1 = static_cast<int>(tips1.size());

    std::vector<int16> tips0;
    tips0.reserve(n_tips - m1);
    {
      int idx1 = 0;
      for (int16 tip = 0; tip < n_tips; ++tip) {
        if (idx1 < m1 && tips1[idx1] == tip) {
          idx1++;
        } else {
          tips0.push_back(tip);
        }
      }
    }
    const int m0 = static_cast<int>(tips0.size());

    for (int ai = 0; ai < m0 - 1; ++ai) {
      for (int bi = ai + 1; bi < m0; ++bi) {
        for (int ci = 0; ci < m1 - 1; ++ci) {
          for (int di = ci + 1; di < m1; ++di) {
            int16 t[4] = {tips0[ai], tips0[bi], tips1[ci], tips1[di]};
            for (int x = 1; x < 4; ++x) {
              int16 key = t[x];
              int y = x - 1;
              while (y >= 0 && t[y] > key) {
                t[y + 1] = t[y];
                y--;
              }
              t[y + 1] = key;
            }

            int32 qi = quartet_index(t[0], t[1], t[2], t[3],
                                     static_cast<int16>(n_tips));

            bool sa = sp[t[0] / 8] & bitmask[t[0] % 8];
            bool sb = sp[t[1] / 8] & bitmask[t[1] % 8];
            bool sc = sp[t[2] / 8] & bitmask[t[2] % 8];
            bool sd = sp[t[3] / 8] & bitmask[t[3] % 8];
            int state = quartet_state_from_sides(sa, sb, sc, sd);
            if (state == 0) continue;

            resolve_count[qi]--;
            if (resolve_count[qi] == 0) {
              int count_j = profile[qi * 4 + consensus_state[qi]];
              // delta_loss = (k - count_0) - (2(k - count_0 - count_j) + count_0)
              //            = -(k - 2*count_j)
              total_loss -= k - 2.0 * count_j;
              consensus_state[qi] = 0;
            }
          }
        }
      }
    }
  }
};


// ============================================================================
// Greedy "best" strategy
// ============================================================================

static void greedy_best(QCGreedyState& st,
                        const std::vector<int>& sort_ord) {
  while (true) {
    Rcpp::checkUserInterrupt();
    double best_ben = 0.0;
    int best_idx = -1;
    bool best_is_add = false;

    for (int si = 0; si < st.M; ++si) {
      int idx = sort_ord[si];
      if (st.incl[idx]) {
        double ben = st.remove_benefit(idx);
        if (ben > best_ben) {
          best_ben = ben;
          best_idx = idx;
          best_is_add = false;
        }
      } else {
        if (!st.is_compatible(idx)) continue;
        double ben = st.add_benefit(idx);
        if (ben > best_ben) {
          best_ben = ben;
          best_idx = idx;
          best_is_add = true;
        }
      }
    }

    if (best_ben <= 0.0 || best_idx < 0) break;

    if (best_is_add) {
      st.do_add(best_idx);
    } else {
      st.do_remove(best_idx);
    }
  }
}


// ============================================================================
// Greedy "first" strategy
// ============================================================================

static void greedy_first(QCGreedyState& st,
                         const std::vector<int>& sort_ord) {
  bool improving = true;
  while (improving) {
    Rcpp::checkUserInterrupt();
    improving = false;
    for (int si = 0; si < st.M; ++si) {
      int idx = sort_ord[si];
      if (st.incl[idx]) {
        if (st.remove_benefit(idx) > 0.0) {
          st.do_remove(idx);
          improving = true;
          break;
        }
      } else {
        if (!st.is_compatible(idx)) continue;
        if (st.add_benefit(idx) > 0.0) {
          st.do_add(idx);
          improving = true;
          break;
        }
      }
    }
  }
}


// ============================================================================
// Main exported function
// ============================================================================

//' Quartet consensus (C++ implementation)
//'
//' @param splits_list List of raw matrices (one per tree), from as.Splits().
//' @param n_tips Number of tips.
//' @param init_majority Logical: TRUE to start from majority-rule splits.
//' @param init_extended Logical: TRUE to start from extended majority splits.
//' @param greedy_best_flag Logical: TRUE for "best", FALSE for "first".
//'
//' @return A list with `included` (logical), `raw_splits` (raw matrix),
//'   and `light_side` (integer).
//' @keywords internal
// [[Rcpp::export]]
List cpp_quartet_consensus(
    const List& splits_list,
    const int n_tips,
    const bool init_majority,
    const bool init_extended,
    const bool greedy_best_flag
) {
  if (n_tips > QC_MAX_TIPS) {
    Rcpp::stop("QuartetConsensus supports at most %d tips.", QC_MAX_TIPS);
  }
  if (n_tips < 4) {
    Rcpp::stop("Need at least 4 tips for quartet consensus.");
  }

  const int n_tree = splits_list.size();

  // ---- Pool unique splits ----
  PooledSplits pool = pool_splits(splits_list, n_tips);
  const int M = pool.n_splits;

  if (M == 0) {
    return List::create(
      Rcpp::Named("included") = LogicalVector(0),
      Rcpp::Named("raw_splits") = RawMatrix(0, 0),
      Rcpp::Named("light_side") = IntegerVector(0)
    );
  }

  // ---- Build quartet profile ----
  std::vector<int> profile = build_quartet_profile(splits_list, n_tips);

  // ---- Compatibility matrix ----
  std::vector<uint8_t> compat = compat_mat(pool);

  // ---- Sort order (by count descending) ----
  std::vector<int> sort_ord(M);
  std::iota(sort_ord.begin(), sort_ord.end(), 0);
  std::sort(sort_ord.begin(), sort_ord.end(),
            [&](int a, int b) { return pool.count[a] > pool.count[b]; });

  // ---- Initialize greedy state ----
  QCGreedyState st(profile, pool, compat, M, n_tips, n_tree);

  // ---- Init from majority or extended majority ----
  if (init_majority || init_extended) {
    double half = n_tree / 2.0;

    if (init_extended) {
      // Extended majority: add splits in order of decreasing count,
      // as long as compatible with all already-included splits
      for (int si = 0; si < M; ++si) {
        int idx = sort_ord[si];
        if (pool.count[idx] > half) {
          st.do_add(idx);
        } else if (st.is_compatible(idx)) {
          st.do_add(idx);
        }
      }
    } else {
      // Majority rule: add only those appearing in > 50% of trees
      for (int i = 0; i < M; ++i) {
        if (pool.count[i] > half) {
          st.do_add(i);
        }
      }
    }
  }

  // ---- Greedy loop ----
  if (greedy_best_flag) {
    greedy_best(st, sort_ord);
  } else {
    greedy_first(st, sort_ord);
  }

  // ---- Build output ----
  LogicalVector incl_r(M);
  for (int i = 0; i < M; ++i) incl_r[i] = st.incl[i] != 0;

  RawMatrix raw_splits(M, pool.n_bytes);
  for (int i = 0; i < M; ++i) {
    const unsigned char* src = pool.split(i);
    for (int j = 0; j < pool.n_bytes; ++j) {
      raw_splits(i, j) = Rbyte(src[j]);
    }
  }

  IntegerVector light_side(M);
  for (int i = 0; i < M; ++i) light_side[i] = pool.light_side[i];

  return List::create(
    Rcpp::Named("included") = incl_r,
    Rcpp::Named("raw_splits") = raw_splits,
    Rcpp::Named("light_side") = light_side
  );
}
