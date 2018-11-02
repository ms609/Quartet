#include "TripletDistanceCalculator.h"
#include "int_stuff.h"

#include <iostream>
#include <fstream>
#include <vector>

#ifndef _MSC_VER
#define _stricmp strcasecmp
#endif

void usage(char *programName) {
  std::cout << "Usage: " << programName << " <input filename> [output filename]" << std::endl
	    << std::endl;
  std::cout << "Where:" << std::endl
	    << "\t" << "<input filename> is the name of a file containing multiple trees in" << std::endl 
	    << "\t" << "Newick format. Each tree should be on a seperate line. In each tree" << std::endl
	    << "\t" << "all leaves should be labeled and all trees should have the same set" << std::endl
	    << "\t" << "of leaf labels." << std::endl;
  std::cout << "\t" << "If [output filename] is specified the output is written to the file"   << std::endl
	    << "\t" << "pointed to (if the file already exists the current content is deleted" << std::endl
	    << "\t" << "first), otherwise the output is written to stdout." << std::endl;
  std::cout << "\t" << "The output is a lower triangular matrix in which the i, j'th entry"  << std::endl
	    << "\t" << "is the pairwise triplet distance between the tree on line i and the" << std::endl
	    << "\t" << "tree on line j in <input filename>." << std::endl << std::endl;
}

int main(int argc, char** argv) {
  if(argc == 1) {
    usage(argv[0]);
    return 0;
  }

  char *inFilename = argv[1];
  char *outFilename = NULL;
  if(argc > 2) 
    outFilename = argv[2];

  TripletDistanceCalculator tripletCalc;
  std::vector<std::vector<INTTYPE_REST> > results = tripletCalc.calculateAllPairsTripletDistance(inFilename);
  
  std::ofstream fout;
  if(outFilename != NULL)
    fout.open(outFilename);
  std::ostream &out = outFilename != NULL ? fout : std::cout; // redirect to std out if no ouput filename given

  for(std::vector<std::vector<INTTYPE_REST> >::const_iterator it1 = results.begin(); it1 != results.end(); ++it1) {
    for(std::vector<INTTYPE_REST>::const_iterator it2 = it1->begin(); it2 != it1->end(); ++it2) {
      out << (*it2) << "\t";
    }
    out << std::endl;
  }

  return 0;
}
