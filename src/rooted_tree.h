#ifndef ROOTED_TREE_H
#define ROOTED_TREE_H

#include <iostream>
#include <vector>
#include <map>
#include <string>
#include "int_stuff.h"
#include "templated_linked_list.h"
class HDT;// Forward declaration...
class RootedTreeFactory; // Forward declaration...

using namespace std;

class RootedTree
{
	public:
		unsigned int level;
		RootedTree *parent;
		TemplatedLinkedList<RootedTree*> *children;
		string name;
		unsigned int numChildren;
		int maxDegree;
		RootedTree *altWorldSelf;
		HDT *hdtLink;
		int color;
		INTTYPE_REST numZeroes;

		RootedTreeFactory *factory;

		// How many leaves beneath you?
		int n;

		void initialize(string name);
		bool isLeaf();
		void addChild(RootedTree *t);
		RootedTree* getParent();
		INTTYPE_REST getUnresolvedTriplets();
		INTTYPE_N4 getUnresolvedQuartets();
		void toDot();
		vector<RootedTree*>* getList();
		void pairAltWorld(RootedTree *t);
		void colorSubtree(int c);
		void markHDTAlternative();
		bool isError();
		RootedTree *contract(RootedTreeFactory *factory = NULL);

	private:
		bool error;

		void toDotImpl();
		void getListImpl(vector<RootedTree*>* list);
		void computeNullChildrenData();
		RootedTree* contractImpl(RootedTreeFactory *factory);
};

#endif