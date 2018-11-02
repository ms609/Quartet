#ifndef HDT_H
#define HDT_H

#include <iostream>
#include <string>
#include "int_stuff.h"
#include "util.h"
#include "rooted_tree_factory.h"
//#include "counting_linked_list.h"
class RootedTree; // forward declaration
class HDTFactory; // forward declaration
class CountingLinkedList; // forward declaration
class CountingLinkedListNumOnly; // forward declaration

#define INITIALIZE_PAREN_AND_SET_LIST(N_NAME, RESET_NAME) {	\
	if (parent->N_NAME == NULL)				\
	{							\
		parent->N_NAME = factory->getLLNO();		\
		parent->N_NAME->resetIterator();		\
		isReset = true;					\
	}							\
	else isReset = parent->RESET_NAME;			\
	theList = parent->N_NAME;				\
	parent->RESET_NAME = false;				\
}

#define NEXT_LEAST_J(LIST) {					\
    if								\
	(							\
		gotoIteratorValueForNumList(LIST, lastJPlus1) &&\
		LIST->getIteratorNum() < j			\
	)							\
		j = LIST->getIteratorNum();			\
}

using namespace std;

class HDT
{
	public:
		enum NodeType {I, C, G, NotConverted};
		void initialize(CountingLinkedList *countingVars, NodeType type, int numD, RootedTree *link = NULL, bool doLink = true);
		static HDT* constructHDT(RootedTree *t, int numD, HDTFactory *copyStuffFromFactory, bool doLink = true);

		void forceLinks();
		void toDot();
		void mark();
		void markAlternative();
		INTTYPE_REST leafCount();
		RootedTree* extractAndGoBack(RootedTreeFactory *rtfactory);
		void updateCounters();
		INTTYPE_REST getResolvedTriplets();
		INTTYPE_REST getUnresolvedTriplets();

		HDT *left, *right;
		HDTFactory *factory;

		bool altMarked;

		// Used for extract+contract to work
		INTTYPE_REST numZeroes;

		// Quartets
		// Summing agreeing/disagreing resolved quartets
		INTTYPE_N4 quartResolvedAgree;
		INTTYPE_N4 quartResolvedAgreeDiag;
		INTTYPE_N4 quartSumE;
		INTTYPE_N4 quartResolvedAgreeUpper;

	private:
		RootedTree *goBackVariable;
		NodeType type, convertedFrom;
		RootedTree *link;
		HDT *parent, *childParent;
		TemplatedLinkedList<HDT*> *children;
		int degree;

		// Soda13 color 0+1+...+d
		CountingLinkedList *countingVars;
		INTTYPE_REST n_circ;
		INTTYPE_REST n_circ_square;

		// Quartets
#ifdef quartetsToo
		INTTYPE_REST n_0_circ;
		INTTYPE_REST n_paren_0_circ;
		INTTYPE_REST n_circ_circ;
		INTTYPE_REST n_square_paren_circ_circ;
		INTTYPE_REST n_paren_circ_circ;
		INTTYPE_REST n_paren_circ_square;
		INTTYPE_REST n_circ_arrow_paren_square_square;
		INTTYPE_REST n_bracket_circ_square;
		INTTYPE_REST n_0_arrow_circ;
		INTTYPE_REST n_circ_arrow_0;
		INTTYPE_REST n_0_arrow_circ_circ;

		// Added by us
		INTTYPE_REST n_circ_arrow_square_square;

		// Added by us for filling out tables
		INTTYPE_REST n_bracket_circ_circ;
		INTTYPE_REST n_paren_circ_paren_square_square;
		INTTYPE_REST n_circ_arrow_circ;
		INTTYPE_REST n_circ_arrow_square;
		INTTYPE_REST n_paren_circ_circ_arrow_square;
		INTTYPE_REST n_bracket_circ_paren_square_square;
		INTTYPE_REST n_circ_arrow_square_arrow_square;
		// New counters for calculating E
		INTTYPE_REST n_circ_square_triangle;
		INTTYPE_REST n_circ_square_arrow_triangle;
		INTTYPE_REST n_circ_arrow_square_triangle;
		INTTYPE_REST n_paren_circ_square_triangle;
		INTTYPE_REST n_0_circ_square;
		INTTYPE_REST n_0_circ_arrow_square;
		INTTYPE_REST n_0_arrow_circ_square;
		INTTYPE_REST n_circ_square_arrow_0;
		INTTYPE_REST n_circ_arrow_0_square;
		INTTYPE_REST n_paren_0_circ_square;
		INTTYPE_REST n_bracket_circ_square_triangle;
		INTTYPE_REST n_bracket_0_circ_square;
#endif

		// Summing resolved/resolved and unresolved/unresolved
		INTTYPE_REST tripResolved;
		INTTYPE_REST tripUnresolved;

		// Marking stuff as changed or updated
		bool up2date;

		static HDT* preFirstRound(RootedTree *t, int numD, bool doLink, HDTFactory *factory);
		HDT* round(HDTFactory *factory);
		inline bool isDownwardsClosed();
		void toDotImpl();
		RootedTree *extractAndGoBackImpl(RootedTree *addToMe, RootedTreeFactory *factory);
		void handleLeaf();
		void handleCCToC();
		void handleIGToC();
		void handleCTransform();
		void handleG();
		
#ifdef quartetsToo
		bool gotoIteratorValueForList(CountingLinkedList *list, unsigned int num);
		enum AddToType {i_j, paren_i_j, j_arrow_i, i_arrow_j, i_paren_i_j, paren_i_paren_i_j, bracket_i_paren_i_j};
		INTTYPE_REST getIteratorValueForNumList(CountingLinkedListNumOnly *list, unsigned int num);
		bool gotoIteratorValueForNumList(CountingLinkedListNumOnly *list, unsigned int num);
		bool hasIteratorForNumListEnded(CountingLinkedListNumOnly *list);
		void addToNumList(CountingLinkedList *parent, AddToType list, unsigned int num, INTTYPE_REST value);
#endif
};

#endif
