#ifndef UNROOTED_TREE_H
#define UNROOTED_TREE_H

#include <iostream>
#include <vector>
#include <string>
#include <algorithm>

#include "rooted_tree.h"
#include "rooted_tree_factory.h"

using namespace std;

typedef struct UnrootedTree
{
	public:
		string name;
		unsigned int level;
		UnrootedTree *dontRecurceOnMe;
		int maxDegree;

		UnrootedTree()
		{
			dontRecurceOnMe = NULL;
			level = maxDegree = 0;
		}

		UnrootedTree(string name)
		{
			dontRecurceOnMe = NULL;
			level = maxDegree = 0;
			this->name = name;
		}

		~UnrootedTree()
		{
			for(vector<UnrootedTree*>::iterator i = edges.begin(); i != edges.end(); i++)
			{
				UnrootedTree *t = *i;
				if (dontRecurceOnMe != t)
				{
					t->dontRecurceOnMe = this;
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
			dontRecurceOnMe = NULL;
			cout << "graph g {" << endl;
			cout << "node[shape=circle,label=\"\"];" << endl;
			toDotImpl();
			cout << "}" << endl;
		}

		bool isLeaf()
		{
			return edges.size() == 1;
		}

		vector<UnrootedTree*>* getList()
		{
			dontRecurceOnMe = NULL;
			vector<UnrootedTree*>* list = new vector<UnrootedTree*>();
			getListImpl(list);
			return list;
		}

		RootedTree* convertToRootedTree(RootedTreeFactory *oldFactory)
		{
			UnrootedTree *t = this;

			// Make sure the root is not a leaf
			// (unless of course there's only 2 elements in which case we can't avoid it)
			if (isLeaf())
			{
				t = edges.front();
			}

			t->dontRecurceOnMe = NULL;
			RootedTreeFactory *factory = new RootedTreeFactory(oldFactory);
			RootedTree *rooted = t->convertToRootedTreeImpl(factory);

			// Make sure the root always recurses on everything! (e.g. so that we can cleanup properly!)
			dontRecurceOnMe = NULL;

			return rooted;
		}

	private:
		vector<UnrootedTree*> edges;

		void toDotImpl()
		{
			cout << "n" << this << "[label=\"" << name << "\"];" << endl;
			/*
			if (isLeaf())
			{
				cout << "n" << this << "[label=\"" << name << "\"];" << endl;
			}
			else
			{
				cout << "n" << this << ";" << endl;
			}
			*/

			for(vector<UnrootedTree*>::iterator i = edges.begin(); i != edges.end(); i++)
			{
				UnrootedTree *t = *i;
				if (t != dontRecurceOnMe)
				{
					t->dontRecurceOnMe = this;
					t->toDotImpl();
					cout << "n" << this << " -- n" << t << ";" << endl;
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
				if (t != dontRecurceOnMe)
				{
					t->dontRecurceOnMe = this;
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
				if (t != dontRecurceOnMe)
				{
					maxDegreeHere++;
					t->dontRecurceOnMe = this;
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