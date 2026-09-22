#ifndef NEX_PARSE_H_
#define NEX_PARSE_H_

#include <fstream>
#include <vector>
#include <stdexcept>  // Quartet: std::runtime_error for file/parse validation

#include "treenode.h"
#include "tree.h"
#include "tree_parse.h"


std::string strip_cr(std::string str) {
	str.erase(remove(str.begin(), str.end(), '\015'), str.end());
	return str;
}


tree* parse_nex(std::string filename) {

	std::ifstream fin(filename.c_str());

	// Quartet: fail cleanly on an unopenable path instead of parsing an empty stream
	if (!fin.is_open()) {
		throw std::runtime_error("Cannot open tree file: " + filename);
	}

	// parse tree
    std::string line;
	getline(fin, line, ';');
	line = strip_cr(line);

	std::vector<tree_node*> nodes;
	// Quartet: nodes holds raw new'd tree_node*; if parse_tree throws mid-parse
	// they would leak. Free them and rethrow (Rcpp turns this into a clean R error).
	try {
		parse_tree(line, nodes);
	} catch (...) {
		for (size_t i = 0; i < nodes.size(); i++) {
			delete nodes[i];
		}
		throw;
	}
	tree* mytree = new tree(nodes);
	return mytree;
}


#endif /* NEX_PARSE_H_ */
