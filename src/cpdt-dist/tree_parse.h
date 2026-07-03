#ifndef TREE_PARSE_H_
#define TREE_PARSE_H_


#include <vector>
#include <string>
#include <cassert>
#include <climits>    // Quartet: INT_MAX for overflow-safe parse_int
#include <stdexcept>  // Quartet: std::runtime_error for input validation
#include <iostream>

#include "treenode.h"
#include "tree.h"


bool isdigit(char c) {
	return c >= '0' && c <= '9';
}

int parse_int(const char*& str) {
	int i = 0;
	while (isdigit(*str)) {
		// Quartet: detect signed-int overflow before it happens (UB under -DNDEBUG)
		if (i > (INT_MAX - 9) / 10) {
			throw std::runtime_error("Taxon number too large in tree string");
		}
		i = i*10 + (*str-'0');
		str++;
	}
	return i;
}


tree_node* parse_tree_support(const char*& str, std::vector<tree_node*>& nodes) {

	// Quartet: real runtime check (asserts are no-ops under -DNDEBUG). Catches
	// empty/no-'(' input before the str++ walks off the end of the string.
	if (*str != '(') {
		throw std::runtime_error("Malformed tree string: expected '('");
	}
	str++;

	int vecpos = nodes.size();
	nodes.push_back(new tree_node(vecpos));

	// Quartet: real runtime check for a valid subtree start ('(' or a digit)
	if (*str != '(' && !isdigit(*str)) {
		throw std::runtime_error("Malformed tree string: expected '(' or digit");
	}
	while (true) {
		tree_node* subtree;
		if (*str == '(') {
			subtree = parse_tree_support(str, nodes);
		} else {
			subtree = new tree_node(nodes.size(), parse_int(str)-1);
			nodes.push_back(subtree);
		}
		nodes[vecpos]->add_child(subtree);
		// Quartet: real runtime check. Must advance str or break; otherwise a
		// stray/missing char leaves the loop stuck, endlessly new-ing nodes
		// (infinite hang + unbounded allocation). Throw stops both that and
		// the walk-off past a missing ')'.
		if (*str == ',') {
			str++;
		} else if (*str == ')') {
			str++;
			break;
		} else {
			throw std::runtime_error("Malformed tree string: expected ',' or ')'");
		}
	}

	return nodes[vecpos];
}


// this (actually the methods above) parses a string representation of a binary tree
// and returns the tree nodes structure (stored in a vector)
void parse_tree(std::string s, std::vector<tree_node*>& nodes) {

	const char* cstr = s.c_str();
	parse_tree_support(cstr, nodes);
}



#endif
