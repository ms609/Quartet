#include "edge_parser.h"

#include <cstdlib>
#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

UnrootedTree* EdgeParser::parseEdge(IntegerMatrix edge) {
  edg = edge;  
  return parse();
}

std::vector<UnrootedTree *> EdgeParser::parseEdges(ListOf<IntegerMatrix> edges) {
  std::vector<UnrootedTree *> trees;
  
  for (int i = 0; i < edges.size(); i++) {
    edg = edges[i];
    trees.push_back(parse());
  }
  
  return trees;
}

UnrootedTree* EdgeParser::parse() {
  nTip = edg(0, 0) - 1;
  it = -1;
  
  UnrootedTree *t = new UnrootedTree(std::to_string(edg(0, 0)));
  ParseBranchSet(t);
  return t;
}

void EdgeParser::ParseBranchSet(UnrootedTree *parent) {
  int degreeHere = 0;
  int subtreeRoot = edg(it + 1, 0);
  int largestDegreeBelow = 0;
  
  while(++it < edg.nrow()) {
    degreeHere++;
    UnrootedTree *child = parseSubTree();
    largestDegreeBelow = max(largestDegreeBelow, child->maxDegree);
    parent->addEdgeTo(child);
    if ((it + 1) >= edg.nrow() || edg(it + 1, 0) != subtreeRoot) break;
  }
  
  parent->maxDegree = max(degreeHere, largestDegreeBelow);
}

UnrootedTree* EdgeParser::parseSubTree() {
  if (edg(it, 1) > nTip) {
    UnrootedTree *internalNode = new UnrootedTree(std::to_string(edg(it, 1)));
    ParseBranchSet(internalNode);
    return internalNode;
  } else {
    return new UnrootedTree(parseName());
  }
}

string EdgeParser::parseName() {
  return std::to_string(edg(it, 1) - 1);
}
