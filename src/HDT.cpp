#include <Rcpp.h>
#include "hdt.h"
#include "rooted_tree.h"
#include "hdt_factory.h"
#include "counting_linked_list.h"

// HDT: Heirarchical Decomposition Tree [Brodal et al. 2013]
// 
// L: a leaf in T,
// I: an internal node in T,
// C: a connected subset of the nodes of T,
// G: a set of subtrees with roots being siblings in 

void HDT::initialize(CountingLinkedList *countingVars, NodeType type, int numD, RootedTree *link, bool doLink)
{
	parent = childParent = left = right = NULL;
	children = NULL;
	convertedFrom = NotConverted;
	goBackVariable = NULL;
	tripResolved = 0;
	tripUnresolved = 0;
	quartResolvedAgree = 0;
	quartResolvedAgreeDiag = 0;
	quartResolvedAgreeUpper = 0;
	// New sum for calculating E
	quartSumE = 0;

	up2date = altMarked = false;

	this->type = type;
	this->link = link;
	if (link != NULL && doLink)
	{
		link->hdtLink = this;
	}
	this->degree = numD;
	this->countingVars = countingVars;
}

INTTYPE_REST HDT::leafCount()
{
	if (countingVars->num == 0) return n_circ + countingVars->n_i;
	return n_circ;
}

void HDT::mark()
{
	up2date = false;

	if (parent == NULL || !parent->up2date) return;
	parent->mark();
}

void HDT::markAlternative()
{
	altMarked = true;

	if (parent == NULL || parent->altMarked) return;
	parent->markAlternative();
}

void HDT::forceLinks()
{
	if (link != NULL)
	{
		link->hdtLink = this;
	}
	if (left != NULL) left->forceLinks();
	if (right != NULL) right->forceLinks();
}

RootedTree* HDT::extractAndGoBack(RootedTreeFactory *rtfactory)
{
	RootedTreeFactory *factory = new RootedTreeFactory(rtfactory);
	extractAndGoBackImpl(NULL, factory);
	return goBackVariable;
}

RootedTree* HDT::extractAndGoBackImpl(RootedTree *addToMe, RootedTreeFactory *factory)
{
	if (this->convertedFrom == C && this->left == NULL && this->right == NULL)
	{
		if (link == NULL)
		{
			// We're a leaf, if no link we're a "there once was x leafs with color 0 here"
			link = factory->getRootedTree("");
			link->numZeroes = n_circ;
			if (countingVars->num == 0) link->numZeroes += countingVars->n_i;
		}
		addToMe->addChild(link);
		goBackVariable = addToMe;
		return addToMe;
	}
	else if (left->type == I && right->type == G)
	{
		// I is newer marked, i.e. if we've got here it's because G is in fact marked!

		// Handle IG -> C (-> G)?
		RootedTree *newNode = factory->getRootedTree("");
		goBackVariable = newNode;
		right->extractAndGoBackImpl(newNode, factory);

		left->altMarked = false;
		right->altMarked = false;

		if (type == C) return newNode;

		// Type = G
		addToMe->addChild(newNode);
		return NULL;
	}
	else if (convertedFrom == C || type == C)
	{
		// Handle CC -> C (-> G)?
		RootedTree *newLeft;
		RootedTree *newRight;
		if (!right->altMarked)
		{
			newLeft = left->extractAndGoBackImpl(NULL, factory);

			newRight = factory->getRootedTree("");
			newRight->numZeroes = right->n_circ;
			if (right->countingVars->num == 0) newRight->numZeroes += right->countingVars->n_i;

			if (type == C)
			{
				RootedTree *realNewRight = factory->getRootedTree("");
				RootedTree *tmp = newRight;
				newRight = realNewRight;
				newRight->addChild(tmp);
			}
			
			right->goBackVariable = newRight;
		}
		else if (!left->altMarked)
		{
			newLeft = factory->getRootedTree("");
			RootedTree *newDummy = factory->getRootedTree("");
			newDummy->numZeroes = left->n_circ;
			if (left->countingVars->num == 0) newDummy->numZeroes += left->countingVars->n_i;
			newLeft->addChild(newDummy);
			left->goBackVariable = newLeft;

			newRight = right->extractAndGoBackImpl(NULL, factory);
		}
		else
		{
			newLeft = left->extractAndGoBackImpl(NULL, factory);
			newRight = right->extractAndGoBackImpl(NULL, factory);
		}
		newLeft->addChild(right->goBackVariable);

		goBackVariable = left->goBackVariable;

		left->altMarked = false;
		right->altMarked = false;

		if (type == C) return newRight;
		// Type G
		addToMe->addChild(goBackVariable);
		return NULL;
	}
	else if (this->type == G)
	{
		// Handle GG->G
		if (!left->altMarked)
		{
			RootedTree *newNode = factory->getRootedTree("");
			newNode->numZeroes = left->n_circ;
			if (left->countingVars->num == 0) newNode->numZeroes += left->countingVars->n_i;

			addToMe->addChild(newNode);
			left->goBackVariable = addToMe;
		}
		else left->extractAndGoBackImpl(addToMe, factory);

		if (!right->altMarked)
		{
			RootedTree *newNode = factory->getRootedTree("");
			newNode->numZeroes = right->n_circ;
			if (right->countingVars->num == 0) newNode->numZeroes += right->countingVars->n_i;

			addToMe->addChild(newNode);
			right->goBackVariable = addToMe;
		}
		else right->extractAndGoBackImpl(addToMe, factory);

		left->altMarked = false;
		right->altMarked = false;

		return NULL;
	}
	else
	{
	  Rcpp::stop("Didn't expect this type combination...");
	  return NULL;
	}
}

void HDT::toDot()
{
	toDotImpl();
}

void HDT::updateCounters()
{
	if (this->convertedFrom == C && this->left == NULL && this->right == NULL)
	{
		handleLeaf();
	}
	else if (this->convertedFrom == C)
	{
		if (left->type == C && right->type == C)
			handleCCToC();
		else
			handleIGToC();

		handleCTransform();
	}
	else if (this->type == C)
	{
		if (left->type == C && right->type == C)
			handleCCToC();
		else
			handleIGToC();
	}
	else if (this->type == G)
	{
		handleG();
	}
	up2date = true;
}

INTTYPE_REST HDT::getResolvedTriplets()
{
	return tripResolved;
}

INTTYPE_REST HDT::getUnresolvedTriplets()
{
	return tripUnresolved;
}

HDT* HDT::constructHDT(RootedTree *t, int numD, HDTFactory *copyStuffFromFactory, bool doLink)
{
	HDTFactory *factory = new HDTFactory(numD, copyStuffFromFactory);
	HDT *hdt = preFirstRound(t, numD, doLink, factory);
	while(hdt->children != NULL)
	{
		hdt = hdt->round(factory);
	}
	hdt->factory = factory;
	hdt->factory->deleteTemplatedLinkedList();
	return hdt;
}

#define ADD_CHILD(PARENT, NEW_CHILD) {                          \
  NEW_CHILD->childParent = PARENT;                              \
  TemplatedLinkedList<HDT*> *newItem = factory->getTemplatedLinkedList();  \
  newItem->data = NEW_CHILD;                                    \
  newItem->next = PARENT->children;                             \
  PARENT->children = newItem;                                   \
}

HDT* HDT::preFirstRound(RootedTree *t, int numD, bool doLink, HDTFactory *factory)
{
	if (t->isLeaf())
	{
		HDT *hdt;
		if (t->numZeroes == 0)
			hdt = factory->getHDT(G, t, doLink);
		else
		{
			hdt = factory->getHDT(G, NULL, doLink);
			hdt->numZeroes = t->numZeroes;
		}
		hdt->convertedFrom = C;
		return hdt;
	}

	// Inner node
	HDT *node = factory->getHDT(I, NULL, doLink);
	for(TemplatedLinkedList<RootedTree*> *i = t->children; i != NULL; i = i->next)
	{
		HDT *child = preFirstRound(i->data, numD, doLink, factory);
		ADD_CHILD(node, child);
	}
	return node;
}

HDT* HDT::round(HDTFactory *factory)
{
	// NOTE: C -> G when parent I etc is moved down.

	// Composition 3: If we're a C we only have 1 child.
	// If that's a C, use CC->C, skip the child and go directly to that-one's 
	// child (if it exists)
	if (type == C && children != NULL && children->next == NULL /*children.size() == 1*/)
	{
		HDT *child = children->data;
		if (child->type == C)
		{
			// CC->C, skip 2nd C and recurse
			HDT *newC = factory->getHDT(C, NULL, false);
			newC->left = this;
			newC->left->parent = newC;
			newC->right = child;
			newC->right->parent = newC;

			// If there's children, there's only 1. 
			// We recurse on that one and add the result to our children list.
			if (child->children != NULL)
			{
				child = child->children->data;
				child->childParent = NULL;
				child = child->round(factory);
				ADD_CHILD(newC, child);
			}
			return newC;
		}
	}

	// Recurse on non-G-children, build GG->G
	TemplatedLinkedList<HDT*> *lastG = NULL;
	int foundGs = 0;
	int downwardsOpenChildren = 0;
	TemplatedLinkedList<HDT*> *prevChild = NULL;
	for(TemplatedLinkedList<HDT*> *i = children; i != NULL; i = i->next)
	{
		// In each round we first transform all downwards closed C componenets
		// being children of I componenets into G componenets
		if (i->data->type == C && type == I && i->data->isDownwardsClosed())
		{
			// Convert to G
			i->data->type = G;
			i->data->convertedFrom = C;
		}

		// Composition 1
		if (i->data->type == G)
		{
			foundGs++;

			// We found 2 G's
			if (lastG != NULL)
			{
				// Merge the two G's by removing one and replacing the other with the 
				// new G that points to the two old ones

				// Replace one with a new one with left, right and parent pointers set
				HDT *newG = factory->getHDT(G, NULL, false);
				newG->left = lastG->data;
				newG->left->parent = newG;
				newG->right = i->data;
				newG->right->parent = newG;
				newG->childParent = this;
				lastG->data = newG;

				// Delete the other
				prevChild->next = i->next;
				i = prevChild;

				// Reset lastG
				lastG = NULL;
			}
			else lastG = i;

			prevChild = i; //here too as we continue...

			// Don't recurse on G's
			continue;
		}
		if (!i->data->isDownwardsClosed()) downwardsOpenChildren++;

		// Recurse and save the "new child"
		i->data = i->data->round(factory);
		i->data->childParent = this;

		prevChild = i;
	}

	// Non-forking I with 1 G component: IG->C (Composition 2)
	if (type == I && downwardsOpenChildren < 2 && foundGs == 1)
	{
		HDT *newC = factory->getHDT(C, NULL, false);
		newC->left = this;
		newC->left->parent = newC;
		newC->right = lastG->data; // We've seen 1 G --- we saved that here
		newC->right->parent = newC;
		for(TemplatedLinkedList<HDT*> *i = children; i != NULL; i = i->next)
		{
			if (i->data != lastG->data)
			{
				ADD_CHILD(newC, i->data);
			}
		}

		return newC;
	}

	return this;
}

bool HDT::isDownwardsClosed()
{
	return children == NULL;
}

void HDT::toDotImpl()
{
	if (left != NULL)
	{
		left->toDotImpl();
	}
	if (right != NULL)
	{
		right->toDotImpl();
	}
}
