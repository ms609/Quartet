#ifndef UTILS_H_
#define UTILS_H_

#include <sstream>

std::string vector_to_string(std::vector<int> v) {
	std::stringstream ss;
	for (unsigned int i = 0; i < v.size(); i++) {
		ss << v[i] << " ";
	}
	return ss.str();
}

inline ull comb2(ull n) {
	return n*(n-1)/2;
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

inline int int_log2(int n) {
	int targetlevel = 0;
	while (n >>= 1) ++targetlevel;
	return targetlevel;
}

void track_leaves_supp(tree_node* node, std::vector<int>& leaves) {
	if (node->is_leaf()) {
		leaves.push_back(node->get_taxa());
	} else {
		for (int i = 0; i < node->get_num_children(); i++) {
			track_leaves_supp(node->get_child(i), leaves);
		}
	}
}
void track_leaves(tree* t, std::vector<int>& leaves) {
	track_leaves_supp(t->get_root(), leaves);
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
