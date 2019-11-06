#ifndef UNROOTED_TREE_H
#define UNROOTED_TREE_H

#include <vector>
#include <string>

#include "rooted_tree.h"
#include "rooted_tree_factory.h"

using namespace std;

typedef struct UnrootedTree
{
	public:
		string name;
		unsigned int level;
		UnrootedTree *dontRecurseOnMe;
		int maxDegree;

		UnrootedTree()
		{
			dontRecurseOnMe = NULL;
			level = maxDegree = 0;
		}

		UnrootedTree(string name)
		{
			dontRecurseOnMe = NULL;
			level = maxDegree = 0;
			this->name = name;
		}

		~UnrootedTree()
		{
			for(vector<UnrootedTree*>::iterator i = edges.begin(); i != edges.end(); i++)
			{
				UnrootedTree *t = *i;
				if (dontRecurseOnMe != t)
				{
					t->dontRecurseOnMe = this;
					delete t;
				}
			}
		}

		void addEdgeTo(UnrootedTree *t)
		{
			edges.push_back(t);
			t->edges.push_back(this);
		}

		void toDot()
		{
			dontRecurseOnMe = NULL;
			toDotImpl();
		}

		bool isLeaf()
		{
			return edges.size() == 1;
		}

		vector<UnrootedTree*>* getList()
		{
			dontRecurseOnMe = NULL;
			vector<UnrootedTree*>* list = new vector<UnrootedTree*>();
			getListImpl(list);
			return list;
		}

		RootedTree* convertToRootedTree(RootedTreeFactory *oldFactory)
		{
			UnrootedTree *t = this;

			// Make sure the root is not a leaf
			// (unless there are only 2 elements, in which case we can't avoid it)
			if (isLeaf())
			{
				t = edges.front();
			}

			t->dontRecurseOnMe = NULL;
			RootedTreeFactory *factory = new RootedTreeFactory(oldFactory);
			RootedTree *rooted = t->convertToRootedTreeImpl(factory);

			// Make sure the root always recurses on everything
			// (e.g. so that we can cleanup properly)
			dontRecurseOnMe = NULL;

			return rooted;
		}

	private:
		vector<UnrootedTree*> edges;

		void toDotImpl()
		{
			for(vector<UnrootedTree*>::iterator i = edges.begin(); i != edges.end(); i++)
			{
				UnrootedTree *t = *i;
				if (t != dontRecurseOnMe)
				{
					t->dontRecurseOnMe = this;
					t->toDotImpl();
				}
			}
		}

		void getListImpl(vector<UnrootedTree*>* list)
		{
			if (isLeaf())
			{
				list->push_back(this);
			}

			for(vector<UnrootedTree*>::iterator i = edges.begin(); i != edges.end(); i++)
			{
				UnrootedTree *t = *i;
				if (t != dontRecurseOnMe)
				{
					t->dontRecurseOnMe = this;
					t->level = level + 1;
					t->getListImpl(list);
				}
			}
		}

		RootedTree* convertToRootedTreeImpl(RootedTreeFactory *factory)
		{
			RootedTree *result = factory->getRootedTree(this->name);
			int maxDegreeChildren = 0;
			int maxDegreeHere = 0;
			for(vector<UnrootedTree*>::iterator i = edges.begin(); i != edges.end(); i++)
			{
				UnrootedTree *t = *i;
				if (t != dontRecurseOnMe)
				{
					maxDegreeHere++;
					t->dontRecurseOnMe = this;
					RootedTree *rt = t->convertToRootedTreeImpl(factory);
					result->addChild(rt);
					maxDegreeChildren = max(maxDegreeChildren, rt->maxDegree);
				}
			}
			result->maxDegree = max(maxDegreeHere, maxDegreeChildren);
			return result;
		}
} UnrootedTree;

#endif
