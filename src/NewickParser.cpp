#include "newick_parser.h"

#include <Rcpp.h>
#include <fstream>
#include <cstdlib>

#include <cctype>
#include <locale>

using namespace std;
using namespace Rcpp;

namespace {

  static inline std::string trim_comment(std::string &s) {
    std::string trimmed = s.substr(0, s.find("%", 0));
    return trimmed;
  }

  static inline std::string &rtrim(std::string &s) {
    s.erase(std::find_if(s.rbegin(), s.rend(), 
                         [](int c) {return !std::isspace(c);}).base(),
                         s.end());
    return s;
  }
  
  static inline bool emptyLine(std::string &line) {
    std::string l = rtrim(line);
    return l == "";
  }

  static inline void eraseWhitespace(std::string &s) {
    s.erase(std::remove_if(s.begin(), s.end(), ::isspace), s.end());
  }
  
}

std::shared_ptr<UnrootedTree> NewickParser::parseFile(const char* filename) {
  // Read file
  ifstream infile;
  Rcout << "OKASI\n";
  // Open file at the end!
  infile.open(filename);
  if (infile) {
    std::string line;
    std::stringstream ss;
    while(true) {
      getline(infile, line);
      line = trim_comment(line);
      line = rtrim(line);
      if(emptyLine(line)) {
        continue;
      }
      ss << line;
      if(infile.eof()) {
        str = ss.str();
        break;
      }
      if(line[line.size()-1] == ';') {
        str = ss.str();
        break;
      }
    }
    infile.close();
    
    // replace all whitespace
    eraseWhitespace(str);
    
    std::shared_ptr<UnrootedTree> t = parse();
    return t;
  } else { // Couldn't open file!
    Rcpp::stop("Nexus Parser couldn't open file");
  }
}

std::shared_ptr<UnrootedTree> NewickParser::parseStr(Rcpp::CharacterVector string_in) {
  str = as<std::string>(string_in);
  eraseWhitespace(str);
  
  std::shared_ptr<UnrootedTree> t = parse();
  return t;

}

std::vector<std::shared_ptr<UnrootedTree> > NewickParser::\
  parseMultiFile(const char *filename) {
  ifstream infile;

  // Open
  infile.open(filename);
  if(infile) {
    std::vector<std::shared_ptr<UnrootedTree> > trees;

    std::string line;
    std::stringstream ss;
    while(true) {
      getline(infile, line);
      if(infile.eof())
        break;
      if(emptyLine(line))
        continue;
      line = trim_comment(line);
      ss << line;
      if(line[line.size()-1] == ';') {
        str = ss.str();
        
        trees.push_back(std::move(parse()));
        ss.str(std::string());
      }    
    }
    
    infile.close();
    return trees;
  } else {
    Rcpp::stop("Nexus Parser couldn't open multifile");
  }
}

std::vector<std::shared_ptr<UnrootedTree> > 
  NewickParser::parseMultiStr(CharacterVector string_in) {
  std::vector<std::shared_ptr<UnrootedTree> > trees;
  std::string line;
  std::stringstream ss;
  for (int i = 0; i < string_in.length(); i++) {
    line = string_in(i);
    if(emptyLine(line))
      continue;
    line = trim_comment(line);
    ss << line;
    if(line[line.size()-1] == ';') {
      str = ss.str();
      
      trees.push_back(parse());
      ss.str(std::string());
    }
  }
  
  return trees;
}

std::shared_ptr<UnrootedTree> NewickParser::parseStr(string inputStr) {
  str = inputStr;
  return parse();
}

bool NewickParser::isError() {
  return parseError;
}

int NewickParser::getPos() {
  if (it == strEnd) {
    Rcpp::stop("Parse error! String ended!");
    parseError = true;
    return -1;
  }
  return distance(str.begin(), it);
}

std::shared_ptr<UnrootedTree>  NewickParser::parse() {
  Rcout << "Beginning to Parse()\n";
  parseError = false;
  it = str.begin();
  strEnd = str.end();
  
  if (*str.rbegin() != ';') 
    return NULL;
  std::shared_ptr<UnrootedTree> t = parseSubTree();
  parseLength();
  if (it == strEnd) {
    Rcpp::stop("Parse error! String is finished before ';'");
    parseError = true;
  } else {
    if (*it != ';') {
      Rcpp::stop("Parse error! Finished before string finished!");
      parseError = true;
    }
    it++;
    if (it != strEnd) {
      Rcpp::stop("Parse error! Finished before string finished!");
      parseError = true;
    }
  }
  Rcout << "Returning from parse()\n";
  return t;
}

std::shared_ptr<UnrootedTree> NewickParser::parseSubTree() {
  if (it == strEnd) {
    Rcpp::stop("Parse error! String ended!");
    parseError = true;
    return make_shared<UnrootedTree>();
  }
  
  Rcout << "Within parseSubTree();\n";
  if (*it == '(') return parseInternal();
  Rcout << "Returning from parseSubTree();\n";
  return make_shared<UnrootedTree>(parseName());
}

std::shared_ptr<UnrootedTree> NewickParser::parseInternal() {
  if (it == strEnd) {
    Rcpp::stop("Parse error! String ended!");
    parseError = true;
    return make_shared<UnrootedTree>();
  }
  
  // Remove '(' char, create internal node, and recurse
  if (*it != '(') {
    Rcpp::stop("Parse error! Expected '('");
    parseError = true;
  }
  it++;
  auto internalNode = make_shared<UnrootedTree>();
  ParseBranchSet(*internalNode);
  
  if (it == strEnd) {
    Rcpp::stop("Parse error! String ended!");
    parseError = true;
    return internalNode;
  }
  
  // Remove ')' char, get name
  if (*it != ')') {
    Rcpp::stop("Parse error! Expected ')'");
    parseError = true;
  }
  Rcout << "it++\n";
  it++;
  if (it == strEnd) {
    Rcpp::stop("Parse error! String is finished...");
    parseError = true;
  }
  internalNode->name = parseName();
  Rcout << "Return from parseInternal();";
  return internalNode;
}

void NewickParser::ParseBranchSet(UnrootedTree& parent) {
  if (it == strEnd) {
    Rcpp::stop("Parse error! String ended!");
    parseError = true;
    return;
  }
  
  Rcout << "Within ParseBranchSet();\n";
  // Parse arbritrarily many branches (i.e. subtrees with lengths)
  int degreeHere = 0;
  int largestDegreeBelow = 0;
  while(true) {
    degreeHere++;
    std::shared_ptr<UnrootedTree> t = parseSubTree();
    largestDegreeBelow = max(largestDegreeBelow, t->maxDegree);
    (&parent)->addEdgeTo(t);
    parseLength();
    Rcout << "...";
    if (it != strEnd && *it == ',')
      it++; // and go again!
    else
      break;
  }
  (&parent)->maxDegree = max(degreeHere, largestDegreeBelow);
  Rcout << "Max(" << degreeHere << ", " << largestDegreeBelow << "): " 
        << (&parent)->maxDegree << "\n";
}

string NewickParser::parseName() {
  if (it == strEnd) {
    Rcpp::stop("Parse error! String ended!");
    parseError = true;
    return "";
  }
  int nameStartPos = getPos();
  int numChars = 0;
  while(true) {
    char c = *it;
    if (c != '(' && c != ')' && c != ',' && c != ':' && c != ';') {
      it++;
      numChars++;
    }
    else break;
    
    if (it == strEnd) {
      Rcpp::stop("Parse error! String ended!");
      parseError = true;
      break;
    }
  }
  return str.substr(nameStartPos, numChars);
}

void NewickParser::parseLength() {
  // Do we start a number?
  if (it == strEnd) {
    Rcpp::stop("Parse error! String ended!");
    parseError = true;
    return;
  }
  if (*it != ':') return;
  
  // Go past ':'
  it++;
  while(true) {
    char c = *it;
    
    // TODO: Should actually check that this is a number (i.e. [0-9\.]*)
    if (c != '(' && c != ')' && c != ',' && c != ':' && c != ';') {
      it++;
    }
    else 
      break;
    if (it == strEnd) {
      Rcpp::stop("Parse error! String ended!");
      parseError = true;
      break;
    }
  }
}
