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
    std::shared_ptr<UnrootedTree> parseEdge(IntegerMatrix edge);
    std::vector<std::shared_ptr<UnrootedTree>> parseEdges(ListOf<IntegerMatrix> edges);

  private:
    std::shared_ptr<UnrootedTree> parseSubTree();
    std::shared_ptr<UnrootedTree> parseInternal();
    void ParseBranchSet(std::shared_ptr<UnrootedTree> parent);
    string parseName();
    std::shared_ptr<UnrootedTree> parse();
    int getPos();
    
    IntegerMatrix edg;
    int nTip;
    int it;
  };
#endif
