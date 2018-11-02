#include <iostream>

#include "int_stuff.h"
#include "QuartetDistanceCalculator.h"

#ifndef _MSC_VER
#define _stricmp strcasecmp
#endif

void usage(char *programName) {
  std::cout << "Usage: " << programName << " [-v] <filename1> <filename2>" << std::endl
	    << std::endl;
  std::cout << "Where: <filename1> and <filename2> point to two files each containing"        << std::endl
	    << "one tree in Newick format. In both trees all leaves should be labeled"        << std::endl
	    << "and the two trees should have the same set of leaf labels."                   << std::endl;
  std::cout << "The quartet distance between the two trees will be printed to stdout."        << std::endl;
  std::cout << "If the -v option is used, the following numbers will be reported (in this"    << std::endl
	    << "order):"                                                                      << std::endl
	    << "\t - The number of leaves in the trees (should be the same for both)."        << std::endl
	    << "\t - The number of quartets in the two trees (n choose 3)."                   << std::endl
	    << "\t - The quartet distance between the two trees."                             << std::endl
	    << "\t - The normalized quartet distance between the two trees."                  << std::endl
	    << "\t - The number of resolved quartets that agree in the two trees."            << std::endl
	    << "\t - The normalized number of resolved quartets that agree in the two trees." << std::endl
	    << "\t - The number of quartets that are unresolved in both trees."               << std::endl
	    << "\t - The normalized number of quartets that are unresolved in both trees."    << std::endl
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

  QuartetDistanceCalculator quartetCalc;
  INTTYPE_N4 dist = quartetCalc.calculateQuartetDistance(filename1, filename2);

  if(dist == -1)
    exit(-1);

  if(!verbose)
    std::cout << dist << std::endl;
  else {
    INTTYPE_N4 resolvedQuartetsAgree = quartetCalc.get_resolvedQuartetsAgree();
    INTTYPE_N4 resolvedQuartetsAgreeDiag = quartetCalc.get_resolvedQuartetsAgreeDiag();
    INTTYPE_N4 resolvedQuartetsDisagree = quartetCalc.get_resolvedQuartetsDisagree();
    INTTYPE_N4 resolvedQuartetsDisagreeDiag = quartetCalc.get_resolvedQuartetsDisagreeDiag();
    INTTYPE_N4 resolvedQuartetsAgreeUpper = quartetCalc.get_resolvedQuartetsAgreeUpper();
    INTTYPE_N4 resolvedQuartetsDisagreeUpper = quartetCalc.get_resolvedQuartetsDisagreeUpper();

    INTTYPE_N4 n = quartetCalc.get_n();
    INTTYPE_N4 totalNoQuartets = quartetCalc.get_totalNoQuartets();
    double dist_norm = double(dist) / double(totalNoQuartets);
    INTTYPE_N4 resAgree = resolvedQuartetsAgree + resolvedQuartetsAgreeDiag + resolvedQuartetsAgreeUpper;
    double resAgree_norm = double(resAgree) / double(totalNoQuartets);
    INTTYPE_N4 unresolvedQuartetsAgree = quartetCalc.get_unresolvedQuartets();
    double unresolvedQuartetsAgree_norm = double(unresolvedQuartetsAgree) / double(totalNoQuartets);
    
    std::cout << n                            << "\t"
	      << totalNoQuartets              << "\t"
	      << dist                         << "\t"
	      << dist_norm                    << "\t"
	      << resAgree                     << "\t"
	      << resAgree_norm                << "\t"
	      << unresolvedQuartetsAgree      << "\t"
	      << unresolvedQuartetsAgree_norm << std::endl;
  }


  return 0;
}
