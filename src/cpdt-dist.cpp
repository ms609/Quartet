#include <Rcpp.h>
#include <TreeTools/renumber_tree.h> /* for preorder */

#include <cstdlib>
#include <iostream>
#include <cassert>
#include <ctime>

#include "cpdt-dist/nex_parse.h"
#include "cpdt-dist/cpdt-dist-bin.h"
#include "cpdt-dist/cpdt-dist.h"

#define NDEBUG

using namespace std;
using namespace Rcpp;



//' Direct entry points to 'cpdt' functions
//' 
//' Functions to calculate rooted triplet distances between pairs of trees.
//' Input is not checked for sanity.
//' 
//' Functions are called from R with functions such as [`CPDTDist`].
//' 
//' @param file1,file2 Paths to files containing a tree or trees in Newick format.
//' 
//' @return The distance between the requested trees.
//' 
//' @author Martin R. Smith, after Ramesh Rajaby
//' 
//' @references \insertRef{Janssen2017jcb}{Quartet}
//' 
//' @keywords internal
//' @export
// [[Rcpp::export]]
IntegerVector cpdt_dist_file(CharacterVector file1,
                             CharacterVector file2) {
    int n1 = file1.size(), n2 = file2.size();
    if (n1 != 1 || n2 != 1) {
        Rcpp::stop("file1 and file2 must be character vectors of length 1");
    }
    
    const char *filename1;
    const char *filename2;
    
    filename1 = CHAR(STRING_ELT(file1, 0));
    filename2 = CHAR(STRING_ELT(file2, 0));
    
    tree* tree1 = parse_nex(filename1);
    tree* tree2 = parse_nex(filename2);
    unsigned long long result = 0;

    if (tree1->is_binary() && tree2->is_binary()) {
        result = cpdt_dist_bin::triplet_distance(tree1, tree2);
    } else {
        result = cpdt_dist::triplet_distance(tree1, tree2);
    }

    return IntegerVector::create(result);
}



tree_node* parse_tree_support(const IntegerMatrix& edge, 
                              const int n_tip, int* row,
                              std::vector<tree_node*>& nodes) {
    
    int vecpos = nodes.size();
    nodes.push_back(new tree_node(vecpos));
    
    if (*row < edge.nrow()) {
        tree_node* subtree;
        const int child = edge(*row, 1);
        ++(*row);
        if (child > n_tip) {
            // Child is a new node, which will occur on the next row
            subtree = parse_tree_support(edge, n_tip, row, nodes);
        } else {
            // Child is a leaf
            subtree = new tree_node(nodes.size(), child - 1);
            nodes.push_back(subtree);
        }
        nodes[vecpos]->add_child(subtree);
    }
    
    return nodes[vecpos];
}


tree* parse_edge(const IntegerVector& parent, const IntegerVector& child) {
    
    IntegerMatrix edge = TreeTools::preorder_edges_and_nodes(parent, child);
    const int n_tip = edge(0, 0) - 1;
    int row = 0;
    std::vector<tree_node*> nodes;
    
    parse_tree_support(edge, n_tip, &row, nodes);
    
    tree* mytree = new tree(nodes);
    return mytree;
}

// [[Rcpp::export]]
IntegerVector cpdt_tree(const IntegerVector parent, const IntegerVector child) {
    tree* mytree = parse_edge(parent, child);
    return IntegerVector(mytree->get_leaves_num());
}
