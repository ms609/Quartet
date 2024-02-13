#ifndef NEX_PARSE_H_
#define NEX_PARSE_H_

#include <fstream>
#include <vector>

#include "treenode.h"
#include "tree.h"
#include "tree_parse.h"


std::string strip_cr(std::string str) {
	str.erase(remove(str.begin(), str.end(), '\015'), str.end());
	return str;
}


tree* parse_nex(std::string filename) {

	std::ifstream fin(filename.c_str());

	// parse tree
    std::string line;
	getline(fin, line, ';');
	line = strip_cr(line);

	std::vector<tree_node*> nodes;
	parse_tree(line, nodes);
	tree* mytree = new tree(nodes);
	return mytree;
}


#endif /* NEX_PARSE_H_ */
