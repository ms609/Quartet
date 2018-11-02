#include <iostream>

#include "int_stuff.h"
#include "TripletDistanceCalculator.h"

#ifndef _MSC_VER
#define _stricmp strcasecmp
#endif

void usage(char *programName) {
  std::cout << "Usage: " << programName << " [-v] <filename1> <filename2>" << std::endl 
	    << std::endl;
  std::cout << "Where <filename1> and <filename2> point to two files each containing one"     << std::endl
	    << "tree in Newick format. In both trees all leaves should be labeled and the"    << std::endl
	    << "two trees should have the same set of leaf labels."                           << std::endl;
  std::cout << "The triplet distance between the two trees will be printed to stdout."        << std::endl;
  std::cout << "If the -v option is used, the following numbers will be reported (in this"    << std::endl
	    << "order):"                                                                      << std::endl
	    << "\t - The number of leaves in the trees (should be the same for both)."        << std::endl
	    << "\t - The number of triplets in the two trees (n choose 3)."                   << std::endl
	    << "\t - The triplet distance between the two trees."                             << std::endl
	    << "\t - The normalized triplet distance between the two trees."                  << std::endl
	    << "\t - The number of resolved triplets that agree in the two trees."            << std::endl
	    << "\t - The normalized number of resolved triplets that agree in the two trees." << std::endl
	    << "\t - The number triplets that are unresolved in both trees."                  << std::endl
	    << "\t - The normalized number triplets that are unresolved in both trees."       << std::endl
	    << std::endl;
}

int main(int argc, char** argv) {
  if(argc == 1) {
    usage(argv[0]);
    return 0;
  }

  bool verbose = false;

  if(argc < 3) {
    std::cerr << "Error: Not enough parameters!" << std::endl;
    usage(argv[0]);
    return -1;
  }

  if(argc == 4 && strcmp(argv[1],"-v") == 0) {
    verbose = true;
  }

  char *filename1 = argv[argc-2];
  char *filename2 = argv[argc-1];

  TripletDistanceCalculator tripletCalc;
  INTTYPE_REST dist = tripletCalc.calculateTripletDistance(filename1, filename2);

  if(dist == -1)
    exit(-1);

  if(!verbose)
    std::cout << dist << std::endl;
  else {
    INTTYPE_REST n = tripletCalc.get_n();
    INTTYPE_REST totalNoTriplets = tripletCalc.get_totalNoTriplets();
    INTTYPE_REST resolved = tripletCalc.get_resolvedTriplets();
    INTTYPE_REST unresolved = tripletCalc.get_unresolvedTriplets();
    double dist_norm = double(dist) / double(totalNoTriplets);
    double resolved_norm = double(resolved) / double(totalNoTriplets);
    double unresolved_norm = double(unresolved) / double(totalNoTriplets);

    std::cout << n               << "\t"
	      << totalNoTriplets << "\t"
	      << dist            << "\t"
	      << dist_norm       << "\t"
	      << resolved        << "\t"
	      << resolved_norm   << "\t"
	      << unresolved      << "\t"
	      << unresolved_norm << std::endl;
  }

  return 0;
}
