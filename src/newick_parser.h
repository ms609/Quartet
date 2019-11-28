#ifndef NEWICK_PARSER_H
	#define NEWICK_PARSER_H

  #include <Rcpp.h>
	#include <string>
	#include "unrooted_tree.h"

	using namespace std;

	class NewickParser
	{
		public:
			~NewickParser() {};
			std::shared_ptr<UnrootedTree> parseStr(string inputStr);
			std::shared_ptr<UnrootedTree> parseStr(Rcpp::CharacterVector string);
			std::shared_ptr<UnrootedTree> parseFile(const char* filename);
			std::vector<std::shared_ptr<UnrootedTree> > parseMultiFile(const char *filename);
			std::vector<std::shared_ptr<UnrootedTree> > parseMultiStr(Rcpp::CharacterVector string);
			bool isError();

		private:
			std::shared_ptr<UnrootedTree> parseSubTree();
		  std::shared_ptr<UnrootedTree> parseInternal();
			void ParseBranchSet(UnrootedTree& parent);
			string parseName();
			void parseLength();
			std::shared_ptr<UnrootedTree> parse();
			int getPos();

			string str;
			string::iterator it;
			string::iterator strEnd;
			bool parseError;
	};
#endif
