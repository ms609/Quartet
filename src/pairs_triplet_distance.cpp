#include "TripletDistanceCalculator.h"
#include "int_stuff.h"
#include "newick_parser.h"
#include "unrooted_tree.h"
#include "rooted_tree.h"

#include <iostream>
#include <fstream>
#include <vector>

#ifndef _MSC_VER
#define _stricmp strcasecmp
#endif

void usage(char *programName) {
  std::cout << "Usage: " << programName << " [-v] <filename1> <filename2> [<output filename>]" << std::endl
	    << std::endl;
  std::cout << "Where: <filename1> and <filename2> point to two files each containing"        << std::endl
	    << "the same number of trees in Newick format. The two trees on line i in"        << std::endl
            << "the two files must have the same set of leaf labels."                         << std::endl;
  std::cout << "The output is a list of numbers, where the i'th number is the triplet"        << std::endl
            << "distance between the two trees on line i in the two files."                   << std::endl;
  std::cout << "If [output filename] is specified the output is written to the file"          << std::endl
	    << "pointed to (if the file already exists the current content is deleted"        << std::endl
	    << "first), otherwise the output is written to stdout."                           << std::endl;
  std::cout << "If the -v option is used, the following numbers will be reported for"         << std::endl
	    << "each pair of trees (in this order):"                                          << std::endl
	    << "\t - The number of leaves in the trees (should be the same for both)."        << std::endl
	    << "\t - The number of triplets in the two trees (n choose 3)."                   << std::endl
	    << "\t - The triplet distance between the two trees."                             << std::endl
	    << "\t - The normalized triplet distance between the two trees."                  << std::endl
	    << "\t - The number of resolved triplets that agree in the two trees."            << std::endl
	    << "\t - The normalized number of resolved triplets that agree in the two trees." << std::endl
	    << "\t - The number of triplets that are unresolved in both trees."               << std::endl
	    << "\t - The normalized number of triplets that are unresolved in both trees."    << std::endl
	    << std::endl;
}

int main(int argc, char** argv) {
  if(argc < 3 || argc > 5) {
    usage(argv[0]);
    exit(-1);
  }

  int c = 1;

  bool verbose = false;
  if(strcmp(argv[c],"-v") == 0) {
    verbose = true;
    c++;
  }

  if(verbose && argc < 4) {
    usage(argv[0]);
    exit(-1);
  }

  char *inFilename1 = argv[c];
  c++;
  char *inFilename2 = argv[c];
  c++;

  char *outFilename = NULL;
  if(c < argc) {
    outFilename = argv[c];
  }

  std::ofstream fout;
  if(outFilename != NULL)
    fout.open(outFilename);
  std::ostream &out = outFilename != NULL ? fout : std::cout; // redirect to std out if no ouput filename given

  NewickParser parser;
 
  std::vector<UnrootedTree *> unrootedTrees1 = parser.parseMultiFile(inFilename1); 
  if(unrootedTrees1.size() == 0 || parser.isError()) {
    std::cerr << "Error: Parsing of \"" << inFilename1 << "\" failed." << endl;
    std::cerr << "Aborting!" << endl;
    std::exit(-1);
  }

  std::vector<UnrootedTree *> unrootedTrees2 = parser.parseMultiFile(inFilename2); 
  if(unrootedTrees2.size() == 0 || parser.isError()) {
    std::cerr << "Error: Parsing of \"" << inFilename2 << "\" failed." << endl;
    std::cerr << "Aborting!" << endl;
    std::exit(-1);
  }

  if(unrootedTrees1.size() != unrootedTrees2.size()) {
    std::cerr << "Error: The two files do not contain the same number of trees." << endl;
    std::cerr << "Aborting!" << endl;
    std::exit(-1);
  }

  TripletDistanceCalculator tripletCalc;

  if(verbose) {
    tripletCalc.pairs_triplet_distance_verbose(out, unrootedTrees1, unrootedTrees2);
  } else {
    const std::vector<INTTYPE_REST> &ptd = tripletCalc.pairs_triplet_distance(unrootedTrees1, unrootedTrees2);
    for(std::vector<INTTYPE_REST>::const_iterator it = ptd.begin(); it != ptd.end(); ++it)
      out << (*it) << std::endl;
  }
}
