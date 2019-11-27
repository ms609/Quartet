#include "edge_parser.h"

#include <cstdlib>
#include <Rcpp.h>

using namespace std;
using namespace Rcpp;

std::shared_ptr<UnrootedTree> EdgeParser::parseEdge(IntegerMatrix edge) {
  edg = edge;
  return parse();
}

std::vector<std::shared_ptr<UnrootedTree>> EdgeParser::\
  parseEdges(ListOf<IntegerMatrix> edges) {
  std::vector<std::shared_ptr<UnrootedTree>> trees;
  
  for (int i = 0; i < edges.size(); i++) {
    edg = edges[i];
    trees.push_back(std::move(parse()));
  }
  
  return trees;
}

std::shared_ptr<UnrootedTree> EdgeParser::parse() {
  nTip = edg(0, 0) - 1;
  it = -1;
  
  std::shared_ptr<UnrootedTree> t = make_shared<UnrootedTree>(std::to_string(edg(0, 0)));
  ParseBranchSet(t);
  return t;
}

void EdgeParser::ParseBranchSet(std::shared_ptr<UnrootedTree> parent) {
  int degreeHere = 0;
  int subtreeRoot = edg(it + 1, 0);
  int largestDegreeBelow = 0;
  
  while(++it < edg.nrow()) {
    degreeHere++;
    std::shared_ptr<UnrootedTree> child = parseSubTree();
    largestDegreeBelow = max(largestDegreeBelow, child->maxDegree);
    parent->addEdgeTo(child.get());
    if ((it + 1) >= edg.nrow() || edg(it + 1, 0) != subtreeRoot) break;
  }
  
  parent->maxDegree = max(degreeHere, largestDegreeBelow);
}

std::shared_ptr<UnrootedTree> EdgeParser::parseSubTree() {
  if (edg(it, 1) > nTip) {
    std::shared_ptr<UnrootedTree> internalNode = 
      make_shared<UnrootedTree>(std::to_string(edg(it, 1)));
    ParseBranchSet(internalNode);
    return internalNode;
  } else {
    return make_shared<UnrootedTree>(parseName());
  }
}

string EdgeParser::parseName() {
  return std::to_string(edg(it, 1) - 1);
}
