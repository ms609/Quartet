#ifndef NEWICK_PARSER_H
	#define NEWICK_PARSER_H

	#include <string>
	#include "unrooted_tree.h"

	using namespace std;

	class NewickParser
	{
		public:
			~NewickParser() {};
			UnrootedTree *parseStr(string inputStr);
			UnrootedTree *parseFile(const char* filename);
			std::vector<UnrootedTree *> parseMultiFile(const char *filename);
			bool isError();

		private:
			UnrootedTree *parseSubTree();
			UnrootedTree *parseInternal();
			void ParseBranchSet(UnrootedTree *parent);
			string parseName();
			void parseLength();
			UnrootedTree *parse();
			int getPos();

			string str;
			string::iterator it;
			string::iterator strEnd;
			bool parseError;
	};
#endif
