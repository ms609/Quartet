#ifndef UNROOTED_TREE_H
#define UNROOTED_TREE_H

#include <Rcpp.h> // For Rcout

#include <memory>
#include <string>
#include <vector>

#include "rooted_tree.h"
#include "rooted_tree_factory.h"

using namespace std;

typedef struct UnrootedTree
{
	public:
		string name;
		unsigned int level;
		shared_ptr<UnrootedTree> dontRecurseOnMe;
		int maxDegree;

		UnrootedTree()
		{
			dontRecurseOnMe = shared_ptr<UnrootedTree>(nullptr);
			level = maxDegree = 0;
		}

		UnrootedTree(string name)
		{
			dontRecurseOnMe = shared_ptr<UnrootedTree>(nullptr);
			level = maxDegree = 0;
			this->name = name;
		}

		~UnrootedTree()
		{
			for(vector<std::shared_ptr<UnrootedTree> >::iterator i = edges.begin();
       i != edges.end(); i++)
			{
			  std::shared_ptr<UnrootedTree> t = *i;
				if (dontRecurseOnMe != t)
				{
					t->dontRecurseOnMe = std::shared_ptr<UnrootedTree>(this);
				}
			}
		}

		void addEdgeTo(shared_ptr<UnrootedTree> t)
		{
			edges.push_back(t);
			t->edges.push_back(std::shared_ptr<UnrootedTree>(this));
		}

		void toDot()
		{
			dontRecurseOnMe = shared_ptr<UnrootedTree>(nullptr);
			toDotImpl();
		}

		bool isLeaf()
		{
			return edges.size() == 1;
		}

		vector<std::shared_ptr<UnrootedTree> >* getList()
		{
			dontRecurseOnMe = NULL;
			vector<std::shared_ptr<UnrootedTree> >* list = new vector<std::shared_ptr<UnrootedTree> >();
			getListImpl(list);
			return list;
		}

		RootedTree* convertToRootedTree(RootedTreeFactory *oldFactory)
		{
		  std::shared_ptr<UnrootedTree> t = std::shared_ptr<UnrootedTree>(this);

			// Make sure the root is not a leaf
			// (unless there are only 2 elements, in which case we can't avoid it)
			if (isLeaf())
			{
				t = edges.front();
			}

			t->dontRecurseOnMe = NULL;
			auto factory = make_shared<RootedTreeFactory>(oldFactory);
			RootedTree *rooted = t->convertToRootedTreeImpl(factory);

			// Make sure the root always recurses on everything
			// (e.g. so that we can cleanup properly)
			dontRecurseOnMe = NULL;

			return rooted;
		}

	private:
		vector<std::shared_ptr<UnrootedTree> > edges;

		void toDotImpl()
		{
			for(vector<std::shared_ptr<UnrootedTree> >::iterator i = edges.begin();
       i != edges.end(); i++)
			{
			  std::shared_ptr<UnrootedTree> t = *i;
				if (t != dontRecurseOnMe)
				{
					t->dontRecurseOnMe = std::shared_ptr<UnrootedTree>(this);
					t->toDotImpl();
				}
			}
		}

		void getListImpl(vector<std::shared_ptr<UnrootedTree> >* list)
		{
			if (isLeaf())
			{
				list->push_back(std::shared_ptr<UnrootedTree>(this));
			}

			for(vector<std::shared_ptr<UnrootedTree> >::iterator i = edges.begin();
       i != edges.end(); i++)
			{
			  std::shared_ptr<UnrootedTree> t = *i;
				if (t != dontRecurseOnMe)
				{
					t->dontRecurseOnMe = std::shared_ptr<UnrootedTree>(this);
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
			for(vector<std::shared_ptr<UnrootedTree> >::iterator i = edges.begin();
       i != edges.end(); i++)
			{
			  std::shared_ptr<UnrootedTree> t = *i;
				if (t != dontRecurseOnMe)
				{
					maxDegreeHere++;
					t->dontRecurseOnMe = std::shared_ptr<UnrootedTree>(this);
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
