#include <Rcpp.h>
using namespace Rcpp;

#include <cstdlib>
#include <iostream>
#include <cassert>
#include <ctime>

#include "cpdt-dist/nex_parse.h"
#include "cpdt-dist/cpdt-dist-bin.h"
#include "cpdt-dist/cpdt-dist.h"

#define NDEBUG

using namespace std;



//' Direct entry points to 'cpdt' functions
//' 
//' Functions to calculate rooted triplet distances between pairs of trees.
//' Input is not checked for sanity.
//' 
//' Functions are called from R with functions such as [`CPDTDist`].
//' 
//' @param t1,t2 Newick format representations of phylogenetic trees.
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
IntegerVector cpdt_dist_pair(CharacterVector t1, CharacterVector t2) {

    tree* tree1 = parse_nex(Rcpp::as<std::string>(t1[0]));
    tree* tree2 = parse_nex(Rcpp::as<std::string>(t2[0]));
    unsigned long long result = 0;

    if (tree1->is_binary() && tree2->is_binary()) {
        result = cpdt_dist_bin::triplet_distance(tree1, tree2);
    } else {
        result = cpdt_dist::triplet_distance(tree1, tree2);
    }

    return IntegerVector(result);
}
