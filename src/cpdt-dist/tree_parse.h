#ifndef TREE_PARSE_H_
#define TREE_PARSE_H_


#include <vector>
#include <string>
#include <cassert>
#include <iostream>

#include "treenode.h"
#include "tree.h"


bool isdigit(char c) {
	return c >= '0' && c <= '9';
}

int parse_int(const char*& str) {
	int i = 0;
	while (isdigit(*str)) {
		i = i*10 + (*str-'0');
		str++;
	}
	return i;
}


tree_node* parse_tree_support(const char*& str, std::vector<tree_node*>& nodes) {

	assert(*str == '(');
	str++;

	int vecpos = nodes.size();
	nodes.push_back(new tree_node(vecpos));

	assert(*str == '(' || isdigit(*str));
	while (true) {
		tree_node* subtree;
		if (*str == '(') {
			subtree = parse_tree_support(str, nodes);
		} else {
			subtree = new tree_node(nodes.size(), parse_int(str)-1);
			nodes.push_back(subtree);
		}
		nodes[vecpos]->add_child(subtree);
		assert(*str == ',' || *str == ')');
		if (*str == ',') {
			str++;
		} else if (*str == ')') {
			str++;
			break;
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
