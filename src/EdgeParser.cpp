#include "edge_parser.h"

#include <Rcpp.h>
#include <cstdlib>

#include <cctype>
#include <locale>

using namespace std;
using namespace Rcpp;

UnrootedTree* EdgeParser::parseEdge(IntegerMatrix edge) {
  edg = edge;  
  return parse();
}

std::vector<UnrootedTree *> EdgeParser::parseEdges(ListOf<IntegerMatrix> edges) {
  std::vector<UnrootedTree *> trees;
  
  for (int i = 0; i < edges.size(); i++) {
    Rcout << " * i = " << i << endl;
    edg = edges[i];
    trees.push_back(parse());
  }
  
  return trees;
}

UnrootedTree* EdgeParser::parse() {
  nTip = edg(0, 0) - 1;
  it = 0;
  
  UnrootedTree *t = new UnrootedTree();
  ParseBranchSet(t);
  return t;
}

void EdgeParser::ParseBranchSet(UnrootedTree *parent) {
  int degreeHere = 0;
  int subtreeRoot = edg(it, 1);
  int largestDegreeBelow = 0;
  
  while(it++ < edg.nrow() - 1) {
    degreeHere++;
    UnrootedTree *t = parseSubTree();
    largestDegreeBelow = max(largestDegreeBelow, t->maxDegree);
    parent->addEdgeTo(t);
    if (edg(it, 0) != subtreeRoot) break;
  }
  
  parent->maxDegree = max(degreeHere, largestDegreeBelow);
}

UnrootedTree* EdgeParser::parseSubTree() {
  if (edg(it, 1) > nTip) {
    UnrootedTree *internalNode = new UnrootedTree();
    ParseBranchSet(internalNode);
    return internalNode;
  } else {
    return new UnrootedTree(parseName());
  }
}

string EdgeParser::parseName() {
  return std::to_string(edg(it, 1) - 1);
}
