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
