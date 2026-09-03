#include <Rcpp.h>
#include <TreeTools/renumber_tree.h> /* for preorder */

#include <climits>
#include <cstdlib>
#include <memory>
#include <vector>
#include <cassert>

#include "cpdt-dist/nex_parse.h"
#include "cpdt-dist/cpdt-dist-bin.h"
#include "cpdt-dist/cpdt-dist.h"

using namespace Rcpp;

// Rooted triplet distance between two parsed trees, dispatching to the
// specialized binary algorithm when both trees are binary.
static unsigned long long cpdt_distance(tree* t1, tree* t2) {
  if (t1->is_binary() && t2->is_binary()) {
    return cpdt_dist_bin::triplet_distance(t1, t2);
  }
  return cpdt_dist::triplet_distance(t1, t2);
}

// Return a count to R as an integer where it fits, falling back to a double
// (exact for counts up to 2^53) when it would overflow R's 32-bit integer.
static SEXP wrap_count(unsigned long long x) {
  if (x > static_cast<unsigned long long>(INT_MAX)) {
    return NumericVector::create(static_cast<double>(x));
  }
  return IntegerVector::create(static_cast<int>(x));
}

//' Direct entry points to 'cpdt' functions
//' 
//' Functions to calculate rooted triplet distances between pairs of trees.
//' Input is not checked for sanity.
//' 
//' Functions are called from R with functions such as [`TripletDistance`].
//' 
//' @param file1,file2 Paths to files containing a tree or trees in Newick format.
//' 
//' @return The distance between the requested trees.
//' 
//' @author Martin R. Smith, after Ramesh Rajaby
//' 
//' @references \insertRef{Jansson2017jcb}{Quartet}
//' 
//' @keywords internal
//' @export
// [[Rcpp::export]]
SEXP cpdt_dist_file(CharacterVector file1,
                    CharacterVector file2) {
    if (file1.size() != 1 || file2.size() != 1) {
        Rcpp::stop("file1 and file2 must be character vectors of length 1");
    }

    const char *filename1 = CHAR(STRING_ELT(file1, 0));
    const char *filename2 = CHAR(STRING_ELT(file2, 0));

    std::unique_ptr<tree> tree1(parse_nex(filename1));
    std::unique_ptr<tree> tree2(parse_nex(filename2));

    return wrap_count(cpdt_distance(tree1.get(), tree2.get()));
}

tree_node* parse_tree_support(const IntegerMatrix& edge, 
                              const int n_tip, int* row,
                              std::vector<tree_node*>& nodes) {
    
    const int vecpos = nodes.size();
    nodes.push_back(new tree_node(vecpos));
    const int starting_node = edge(*row, 0);
    
    while (*row < edge.nrow() && edge(*row, 0) >= starting_node) {
        tree_node* subtree;
        const int child = edge(*row, 1);
        ++(*row);
        if (child > n_tip) {
            subtree = parse_tree_support(edge, n_tip, row, nodes);
        } else {
            subtree = new tree_node(nodes.size(), child - 1);
            nodes.push_back(subtree);
        }
        nodes[vecpos]->add_child(subtree);
    }
    
    return nodes[vecpos];
}


tree* parse_edge(const IntegerVector& parent, const IntegerVector& child) {
    
    IntegerMatrix edge = TreeTools::preorder_edges_and_nodes(parent, child);
    if (edge.nrow() < 1) {
        Rcpp::stop("`edge` must have at least one row");
    }
    const int n_tip = edge(0, 0) - 1;
    int row = 0;
    std::vector<tree_node*> nodes;
    
    parse_tree_support(edge, n_tip, &row, nodes);
    
    tree* mytree = new tree(nodes);
    return mytree;
}

// [[Rcpp::export]]
SEXP cpdt_pair(const IntegerVector parent1, const IntegerVector child1,
               const IntegerVector parent2, const IntegerVector child2) {

    std::unique_ptr<tree> tree1(parse_edge(parent1, child1));
    std::unique_ptr<tree> tree2(parse_edge(parent2, child2));

    return wrap_count(cpdt_distance(tree1.get(), tree2.get()));
}

// [[Rcpp::export]]
List cpdt_tree(const List r_tree) {
    const IntegerMatrix edge = r_tree["edge"];
    tree* mytree = parse_edge(edge(_, 0), edge(_, 1));
    List out = List::create(Named("ntip") = mytree->get_leaves_num(),
                            _["nnode"] = mytree->get_nodes_num(),
                            _["binary"] = mytree->is_binary(),
                            _["string"] = mytree->to_string());
    delete mytree;
    return out;
}

// All pairwise rooted triplet distances within a list of trees.
// `edges` is a list of edge matrices (columns: parent, child) whose leaves
// have already been renumbered to a common ordering on the R side, so that
// leaf i denotes the same taxon in every tree.  Each tree is parsed once and
// reused across all pairs, avoiding the O(n^2) re-parsing of the naive R loop.
// [[Rcpp::export]]
SEXP cpdt_all_pairs(List edges) {
    const R_xlen_t n = edges.size();
    std::vector<tree*> trees(n, nullptr);

    // Guarantee every parsed tree is freed, even if a later step throws.
    struct TreeGuard {
        std::vector<tree*>& v;
        ~TreeGuard() { for (tree* t : v) delete t; }
    } guard{trees};

    for (R_xlen_t i = 0; i < n; ++i) {
        IntegerMatrix edge = edges[i];
        trees[i] = parse_edge(edge(_, 0), edge(_, 1));
    }

    std::vector<unsigned long long> dist(static_cast<size_t>(n) * n, 0ULL);
    unsigned long long maxDist = 0;
    for (R_xlen_t r = 1; r < n; ++r) {
        for (R_xlen_t c = 0; c < r; ++c) {
            const unsigned long long d = cpdt_distance(trees[r], trees[c]);
            dist[r + c * n] = d;
            dist[c + r * n] = d;
            if (d > maxDist) maxDist = d;
        }
    }

    const R_xlen_t nn = n * n;
    if (maxDist > static_cast<unsigned long long>(INT_MAX)) {
        NumericMatrix out(n, n);
        for (R_xlen_t i = 0; i < nn; ++i) out[i] = static_cast<double>(dist[i]);
        return out;
    }
    IntegerMatrix out(n, n);
    for (R_xlen_t i = 0; i < nn; ++i) out[i] = static_cast<int>(dist[i]);
    return out;
}
