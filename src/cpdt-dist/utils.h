#ifndef UTILS_H_
#define UTILS_H_

#include <stdexcept> /* for std::runtime_error */
#include <vector>

#include "bit.h"      /* for ull */
#include "tree.h"     /* for tree, tree_node */
#include "treenode.h"

inline ull comb2(ull n) {
	return n*(n-1)/2;
}

// Change in comb2() when `b` reds are removed from a node that currently holds
// `a` reds, i.e. exactly comb2(a) - comb2(a - b).  This is the delta the CPDT
// Fenwick "reds2"/"reds2p" trees are decremented by when de-colouring.
//
// A2-05: the caller's structural invariant is a >= b (you never de-colour more
// reds than are present in a subtree -- colour and de-colour come in matched
// pairs, and `b` counts leaves that are currently red).  If that invariant ever
// failed, the *unsigned* subtraction a - b would wrap to ~2^64 and comb2() --
// being quadratic -- would yield a garbage delta that Fenwick's modular (mod
// 2^64) group arithmetic could NOT later cancel out, unlike the linear +/-reds
// updates.  The result would be a silently-wrong triplet distance that UBSAN
// does not flag (unsigned wrap is defined behaviour).
//
// The guard converts any future invariant violation into a clean, loud error
// instead of a silent wrong answer.  It is compiled in unconditionally, so it
// protects CRAN's -DNDEBUG builds where a bare assert() is a no-op.  On every
// valid input the branch is not taken and the returned expression is textually
// identical to the original code, so no computed distance can change.
inline ull comb2_removed(ull a, ull b) {
	if (b > a) {
		throw std::runtime_error(
			"cpdt: attempted to de-colour more reds than are present");
	}
	return comb2(a) - comb2(a - b); // b <= a, so (a - b) cannot wrap
}
inline ull comb3(ull n) {
	ull n1 = n-1, n2 = n-2; 
	if (n%2 == 0) {
		n /= 2;
	} else {
		n1 /= 2;
	}
	if (n%3 == 0) {
		n /= 3;
	} else if (n1%3 == 0) {
		n1 /= 3;
	} else {
		n2 /= 3;
	}
	return n*n1*n2;
}

void track_leaves(tree_node* node, std::vector<int>& leaves,
		std::vector<int>& node_range_begin, std::vector<int>& node_range_end) {
	node_range_begin[node->get_id()] = leaves.size();
	if (node->is_leaf()) {
		leaves.push_back(node->get_taxa());
	} else {
		for (int i = 0; i < node->get_num_children(); i++) {
			track_leaves(node->get_child(i), leaves, node_range_begin, node_range_end);
		}
	}
	node_range_end[node->get_id()] = leaves.size();
}
void track_leaves(tree* t, std::vector<int>& leaves,
		std::vector<int>& node_range_begin, std::vector<int>& node_range_end) {
	node_range_begin.resize(t->get_nodes_num());
	node_range_end.resize(t->get_nodes_num());
	track_leaves(t->get_root(), leaves, node_range_begin, node_range_end);
}

int count_leaves(tree_node* node, std::vector<int>& counts) {
	if (node->is_leaf()) {
		return counts[node->get_id()] = 1;
	}
	for (int i = 0; i < node->get_num_children(); i++) {
		counts[node->get_id()] += count_leaves(node->get_child(i), counts);
	}
	return counts[node->get_id()];
}
void count_leaves(tree* t, std::vector<int>& counts) {
	counts.resize(t->get_nodes_num(), 0);
	count_leaves(t->get_root(), counts);
}

#endif /* UTILS_H_ */
