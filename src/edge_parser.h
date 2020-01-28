#ifndef EDGE_PARSER_H
  #define EDGE_PARSER_H
  
  #include <Rcpp.h>
  #include "unrooted_tree.h"
  
  using namespace std;
  using namespace Rcpp;
  
  class EdgeParser
  {
  public:
    ~EdgeParser() {};
    UnrootedTree *parseEdge(IntegerMatrix edge);
    std::vector<UnrootedTree *> parseEdges(ListOf<IntegerMatrix> edges);

  private:
    UnrootedTree *parseSubTree();
    UnrootedTree *parseInternal();
    void ParseBranchSet(UnrootedTree *parent);
    string parseName();
    UnrootedTree *parse();
    int getPos();
    
    IntegerMatrix edg;
    int nTip;
    int it;
  };
#endif
