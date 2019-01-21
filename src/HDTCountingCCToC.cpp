#include <Rcpp.h>
#include "hdt.h"
#include "rooted_tree.h"
#include "hdt_factory.h"
#include "counting_linked_list.h"

void HDT::handleCCToC()
{
	// Recurse (if not I child) (we really _should_ have both children!)
	if (!left->up2date) left->updateCounters();
	if (!right->up2date) right->updateCounters();

	// NOTE: We generate this with c2 (the higher one) being the left child and c1 (the lower one) being the right child
	HDT *c1 = this->right;
	HDT *c2 = this->left;

	// n_i is just the sum of the 2 children, thus n_circ is also just the sum of the 2 children
	n_circ = c1->n_circ + c2->n_circ;

	// Initialize sum to 0 so we can increment it in the loop
	n_circ_square = 0;

	// Quartets
	// Initialize sums to 0 so we can increment it in the loop
	n_circ_circ = 0;
	n_square_paren_circ_circ = 0;
	n_paren_circ_circ = 0;
	n_paren_circ_square = 0;
	n_circ_arrow_paren_square_square = 0;
	n_bracket_circ_square = 0;
	n_circ_arrow_square_square = 0; // Added by Sand et al. (2014)

	// Not dependent on i
	n_0_circ = c1->n_0_circ + c2->n_0_circ;
	n_paren_0_circ = c1->n_paren_0_circ + c2->n_paren_0_circ;

	// Added by Sand et al. (2014) for filling out tables (initialize)
	n_bracket_circ_circ = 0;
	n_paren_circ_paren_square_square = 0;
	n_circ_arrow_circ = 0;
	n_circ_arrow_square = 0;
	n_paren_circ_circ_arrow_square = 0;
	n_bracket_circ_paren_square_square = 0;
	n_circ_arrow_square_arrow_square = 0;

	// Fetch the childrens "result counting data"
	quartResolvedAgree = c1->quartResolvedAgree + c2->quartResolvedAgree;
	quartResolvedAgreeDiag = c1->quartResolvedAgreeDiag + c2->quartResolvedAgreeDiag;
	quartResolvedAgreeUpper = c1->quartResolvedAgreeUpper + c2->quartResolvedAgreeUpper;

	// Initialize sums and stuff for calculating E
	n_circ_square_triangle = c1->n_circ_square_triangle + c2->n_circ_square_triangle;
	n_circ_square_arrow_triangle = 0;
	n_circ_arrow_square_triangle = 0;
	n_paren_circ_square_triangle = c1->n_paren_circ_square_triangle + c2->n_paren_circ_square_triangle;
	n_0_circ_square = c1->n_0_circ_square + c2->n_0_circ_square;
	n_0_circ_arrow_square = 0;
	n_0_arrow_circ_square = 0;
	n_circ_square_arrow_0 = 0;
	n_circ_arrow_0_square = 0;
	n_paren_0_circ_square = c1->n_paren_0_circ_square + c2->n_paren_0_circ_square;
	n_bracket_circ_square_triangle = 0;
	n_bracket_0_circ_square = 0;

	// Fetch sum for calculating E from children
	quartSumE = c1->quartSumE + c2->quartSumE;

	// Fetch the childrens "result counting data"
	tripResolved = c1->tripResolved + c2->tripResolved;
	tripUnresolved = c1->tripUnresolved + c2->tripUnresolved;

	// Pointer stuff for walking over all children's data
	CountingLinkedList *c1Next = c1->countingVars;
	CountingLinkedList *c1Count = NULL;
	CountingLinkedList *c2Next = c2->countingVars;
	CountingLinkedList *c2Count = NULL;
	CountingLinkedList *ourCount = countingVars;

	// Quartets
	INTTYPE_REST c1Zero = 0;
	INTTYPE_REST c2Zero = 0;

	// The list is sorted, i.e. if zero's there it's the first one!
	if (c1Next->num == 0) c1Zero = c1Next->n_i;
	if (c2Next->num == 0) c2Zero = c2Next->n_i;

	// Not dependent on i (begin)
	n_0_arrow_circ = c1->n_0_arrow_circ + c2->n_0_arrow_circ + 
		c1Zero * c2->n_circ;

	n_circ_arrow_0 = c1->n_circ_arrow_0 + c2->n_circ_arrow_0 +
		c1->n_circ * c2Zero;

	n_0_arrow_circ_circ = c1->n_0_arrow_circ_circ + c2->n_0_arrow_circ_circ +
		c1Zero * c2->n_circ_circ;
	// Not dependent on i (end)

	while (c1Next != NULL || c2Next != NULL)
	{
		if (c2Next == NULL || (c1Next != NULL && c1Next->num < c2Next->num))
		{
			// Operate only on c1
			c1Count = c1Next;
			c2Count = &CountingLinkedList::dummyLL;
			ourCount->num = c1Count->num;

			if (c1Next->type == CountingLinkedList::End) c1Next = NULL;
			else c1Next = c1Next->next;
		}
		else if (c1Next == NULL || (c2Next != NULL && c2Next->num < c1Next->num))
		{
			// Operate only on c2
			c2Count = c2Next;
			c1Count = &CountingLinkedList::dummyLL;
			ourCount->num = c2Count->num;

			if (c2Next->type == CountingLinkedList::End) c2Next = NULL;
			else c2Next = c2Next->next;
		}
		else //c1Count->num == c2Count->num
		{
			c1Count = c1Next;
			c2Count = c2Next;
			ourCount->num = c1Count->num;

			if (c1Next->type == CountingLinkedList::End) c1Next = NULL;
			else c1Next = c1Next->next;
			if (c2Next->type == CountingLinkedList::End) c2Next = NULL;
			else c2Next = c2Next->next;
		}

		// Update counters (triplets)
		ourCount->n_i = c1Count->n_i + c2Count->n_i;
		if (ourCount->num == 0)
		{
			// Go to next one => We're done!
			if (c1Next == NULL && c2Next == NULL)
				ourCount->type = CountingLinkedList::End;
			else
			{
				// Go to next one (there's more!)
				ourCount->type = CountingLinkedList::Regular;
				if (ourCount->next == NULL) ourCount->next = factory->getLL();
				ourCount = ourCount->next;				
				c1Count = c1Next;
				c2Count = c2Next;
			}

			continue;
		}
		ourCount->n_i_circ = c1Count->n_i_circ + c2Count->n_i_circ;
		ourCount->n_paren_ii = c1Count->n_paren_ii + c2Count->n_paren_ii;
		ourCount->n_i_arrow_circ = c1Count->n_i_arrow_circ + c2Count->n_i_arrow_circ +
			c1Count->n_i * (c2->n_circ - c2Count->n_i);

		// Quartets
		// 2nd group in figure 12 (quartets only)
		ourCount->n_0_i = c1Count->n_0_i + c2Count->n_0_i;
		ourCount->n_ii = c1Count->n_ii + c2Count->n_ii;
		ourCount->n_0_paren_ii = c1Count->n_0_paren_ii + c2Count->n_0_paren_ii;
		ourCount->n_circ_paren_ii = c1Count->n_circ_paren_ii + c2Count->n_circ_paren_ii;
		ourCount->n_i_paren_0_circ = c1Count->n_i_paren_0_circ + c2Count->n_i_paren_0_circ;
		ourCount->n_i_paren_circ_circ = c1Count->n_i_paren_circ_circ + c2Count->n_i_paren_circ_circ;
		ourCount->n_i_paren_circ_square = c1Count->n_i_paren_circ_square + c2Count->n_i_paren_circ_square;

		// In the box
		ourCount->n_bracket_i_circ = ourCount->n_i * (n_circ - ourCount->n_i);

		// 3rd group in figure 12 (quartets only)
		ourCount->n_paren_0_i = c1Count->n_paren_0_i + c2Count->n_paren_0_i;
		ourCount->n_paren_i_circ = c1Count->n_paren_i_circ + c2Count->n_paren_i_circ;
		ourCount->n_paren_0_paren_ii = c1Count->n_paren_0_paren_ii + c2Count->n_paren_0_paren_ii;
		ourCount->n_paren_circ_paren_ii = c1Count->n_paren_circ_paren_ii + c2Count->n_paren_circ_paren_ii;
		ourCount->n_paren_i_paren_0_circ = c1Count->n_paren_i_paren_0_circ + c2Count->n_paren_i_paren_0_circ;

		// 4th group in figure 12 (quartets only)
		ourCount->n_bracket_0_paren_ii = c1Count->n_bracket_0_paren_ii + c2Count->n_bracket_0_paren_ii +
			Util::binom2(c1Count->n_i) * c2Zero +
			c1Zero * c2Count->n_paren_ii +
			c1Count->n_i * c2Count->n_i_arrow_0;

		ourCount->n_bracket_circ_paren_ii = c1Count->n_bracket_circ_paren_ii + c2Count->n_bracket_circ_paren_ii +
			Util::binom2(c1Count->n_i) * (c2->n_circ - c2Count->n_i) +
			(c1->n_circ - c1Count->n_i) * c2Count->n_paren_ii +
			c1Count->n_i * c2Count->n_i_arrow_circ;

		ourCount->n_bracket_i_paren_0_circ = c1Count->n_bracket_i_paren_0_circ + c2Count->n_bracket_i_paren_0_circ +
			c1Zero * (c1->n_circ - c1Count->n_i) * c2Count->n_i +
			c1Count->n_i * (c2->n_paren_0_circ - c2Count->n_paren_0_i) +
			c1Zero * c2Count->n_circ_arrow_i +
			(c1->n_circ - c1Count->n_i) * c2Count->n_0_arrow_i;

		// 5th group in figure 12 (quartets only)
		ourCount->n_0_arrow_i = c1Count->n_0_arrow_i + c2Count->n_0_arrow_i +
			c1Zero * c2Count->n_i;

		ourCount->n_i_arrow_0 = c1Count->n_i_arrow_0 + c2Count->n_i_arrow_0 +
			c1Count->n_i * c2Zero;

		ourCount->n_i_arrow_i = c1Count->n_i_arrow_i + c2Count->n_i_arrow_i +
			c1Count->n_i * c2Count->n_i;

		ourCount->n_circ_arrow_i = c1Count->n_circ_arrow_i + c2Count->n_circ_arrow_i +
			(c1->n_circ - c1Count->n_i) * c2Count->n_i;

		ourCount->n_0_arrow_paren_ii = c1Count->n_0_arrow_paren_ii + c2Count->n_0_arrow_paren_ii +
			c1Zero * c2Count->n_paren_ii;

		// NOTICE: THIS HAS CHANGED FROM THE ARTICLE!
		ourCount->n_i_arrow_paren_0_circ = c1Count->n_i_arrow_paren_0_circ + c2Count->n_i_arrow_paren_0_circ +
			c1Count->n_i * (c2->n_paren_0_circ - c2Count->n_paren_0_i);

		ourCount->n_i_arrow_paren_circ_square = c1Count->n_i_arrow_paren_circ_square + c2Count->n_i_arrow_paren_circ_square +
			c1Count->n_i * (c2->n_paren_circ_square - c2Count->n_paren_i_circ);

		ourCount->n_circ_arrow_paren_ii = c1Count->n_circ_arrow_paren_ii + c2Count->n_circ_arrow_paren_ii +
			(c1->n_circ - c1Count->n_i) * c2Count->n_paren_ii;

		ourCount->n_i_arrow_0_circ = c1Count->n_i_arrow_0_circ + c2Count->n_i_arrow_0_circ +
			c1Count->n_i * (c2->n_0_circ - c2Count->n_0_i);

		ourCount->n_i_arrow_circ_circ = c1Count->n_i_arrow_circ_circ + c2Count->n_i_arrow_circ_circ +
			c1Count->n_i * (c2->n_circ_circ - c2Count->n_ii);

		ourCount->n_i_arrow_circ_square = c1Count->n_i_arrow_circ_square + c2Count->n_i_arrow_circ_square +
			c1Count->n_i * (c2->n_circ_square - c2Count->n_i_circ);

		ourCount->n_circ_arrow_ii = c1Count->n_circ_arrow_ii + c2Count->n_circ_arrow_ii +
			(c1->n_circ - c1Count->n_i) * c2Count->n_ii;

		ourCount->n_paren_ii_arrow_0 = c1Count->n_paren_ii_arrow_0 + c2Count->n_paren_ii_arrow_0 +
			c1Count->n_paren_ii * c2Zero;

		ourCount->n_paren_ii_arrow_circ = c1Count->n_paren_ii_arrow_circ + c2Count->n_paren_ii_arrow_circ +
			c1Count->n_paren_ii * (c2->n_circ - c2Count->n_i);

		ourCount->n_paren_circ_circ_arrow_i = c1Count->n_paren_circ_circ_arrow_i + c2Count->n_paren_circ_circ_arrow_i +
			(c1->n_paren_circ_circ - c1Count->n_paren_ii) * c2Count->n_i;

		ourCount->n_0_arrow_i_arrow_i = c1Count->n_0_arrow_i_arrow_i + c2Count->n_0_arrow_i_arrow_i +
			c1Zero * c2Count->n_i_arrow_i +
			c1Count->n_0_arrow_i * c2Count->n_i;

		ourCount->n_i_arrow_circ_arrow_0 = c1Count->n_i_arrow_circ_arrow_0 + c2Count->n_i_arrow_circ_arrow_0 +
			c1Count->n_i * (c2->n_circ_arrow_0 - c2Count->n_i_arrow_0) +
			c1Count->n_i_arrow_circ * c2Zero;

		ourCount->n_i_arrow_0_arrow_circ = c1Count->n_i_arrow_0_arrow_circ + c2Count->n_i_arrow_0_arrow_circ +
			c1Count->n_i * (c2->n_0_arrow_circ - c2Count->n_0_arrow_i) +
			c1Count->n_i_arrow_0 * (c2->n_circ - c2Count->n_i);

		ourCount->n_circ_arrow_i_arrow_i = c1Count->n_circ_arrow_i_arrow_i + c2Count->n_circ_arrow_i_arrow_i +
			(c1->n_circ - c1Count->n_i) * c2Count->n_i_arrow_i +
			c1Count->n_circ_arrow_i * c2Count->n_i;

		// Added by Sand et al. (2014)
		ourCount->n_i_arrow_paren_circ_circ = c1Count->n_i_arrow_paren_circ_circ + c2Count->n_i_arrow_paren_circ_circ +
			c1Count->n_i * (c2->n_paren_circ_circ - c2Count->n_paren_ii);

		ourCount->n_0_arrow_ii = c1Count->n_0_arrow_ii + c2Count->n_0_arrow_ii +
			c1Zero * c2Count->n_ii;

		ourCount->n_paren_0_circ_arrow_i = c1Count->n_paren_0_circ_arrow_i + c2Count->n_paren_0_circ_arrow_i +
			(c1->n_paren_0_circ - c1Count->n_paren_0_i) * c2Count->n_i;

		// Figure 15 counters (part 1-4, all with j) & Figure 16 sums (with j)
		ourCount->resetIterator();
		c1Count->resetIterator();
		c2Count->resetIterator();
		c1->countingVars->resetIterator();
		c2->countingVars->resetIterator();

		unsigned int wantedMax = degree+1;
		unsigned int lastJPlus1 = 1;
		if (ourCount->n_i_j != NULL) ourCount->n_i_j->type = CountingLinkedListNumOnly::SkipAndEnd;
		if (ourCount->n_j_arrow_i != NULL) ourCount->n_j_arrow_i->type = CountingLinkedListNumOnly::SkipAndEnd;
		if (ourCount->n_i_arrow_j != NULL) ourCount->n_i_arrow_j->type = CountingLinkedListNumOnly::SkipAndEnd;

		// Added by Sand et al. (2014) for filling out tables
		// A
		ourCount->n_bracket_i_paren_circ_circ = c1Count->n_bracket_i_paren_circ_circ + c2Count->n_bracket_i_paren_circ_circ + 
			(c1->n_bracket_circ_circ - Util::binom2(c1Count->n_i)) * c2Count->n_i + c1Count->n_i * (c2->n_paren_circ_circ - c2Count->n_paren_ii);

		ourCount->n_bracket_i_paren_circ_square = c1Count->n_bracket_i_paren_circ_square + c2Count->n_bracket_i_paren_circ_square +
			(c1->n_bracket_circ_square - c1Count->n_bracket_i_circ) * c2Count->n_i + c1Count->n_i * (c2->n_paren_circ_square - c2Count->n_paren_i_circ);

		ourCount->n_i_arrow_circ_arrow_circ = c1Count->n_i_arrow_circ_arrow_circ + c2Count->n_i_arrow_circ_arrow_circ +
			c1Count->n_i * (c2->n_circ_arrow_circ - c2Count->n_i_arrow_i);

		ourCount->n_i_arrow_circ_arrow_square = c1Count->n_i_arrow_circ_arrow_square + c2Count->n_i_arrow_circ_arrow_square +
			c1Count->n_i * (c2->n_circ_arrow_square - c2Count->n_i_arrow_circ - c2Count->n_circ_arrow_i);

		// E
		ourCount->n_i_circ_arrow_square = c1Count->n_i_circ_arrow_square + c2Count->n_i_circ_arrow_square; // More below

		ourCount->n_bracket_i_circ_square = c1Count->n_bracket_i_circ_square + c2Count->n_bracket_i_circ_square +
			c1Count->n_i * (c2->n_circ_square - c2Count->n_i_circ);

		while (true)
		{
			unsigned int j = wantedMax;
			NEXT_LEAST_J(c1Count->n_i_j)					NEXT_LEAST_J(c2Count->n_i_j)
			NEXT_LEAST_J(c1Count->n_j_arrow_i)				NEXT_LEAST_J(c2Count->n_j_arrow_i)
			NEXT_LEAST_J(c1Count->n_i_arrow_j)				NEXT_LEAST_J(c2Count->n_i_arrow_j)

			if (gotoIteratorValueForList(c1->countingVars, lastJPlus1) && c1->countingVars->getIteratorNum() < j) j = c1->countingVars->getIteratorNum();
			if (gotoIteratorValueForList(c2->countingVars, lastJPlus1) && c2->countingVars->getIteratorNum() < j) j = c2->countingVars->getIteratorNum();

			if (j >= wantedMax) break;

			// n_i_j
			INTTYPE_REST new_n_i_j = getIteratorValueForNumList(c1Count->n_i_j, j) + getIteratorValueForNumList(c2Count->n_i_j, j);
			addToNumList(ourCount, i_j, j, new_n_i_j);

			// n_j_arrow_i
			INTTYPE_REST new_n_j_arrow_i = getIteratorValueForNumList(c1Count->n_j_arrow_i, j) + getIteratorValueForNumList(c2Count->n_j_arrow_i, j) +
				c1->countingVars->getIteratorValue(j)->n_i * c2Count->n_i;
			addToNumList(ourCount, j_arrow_i, j, new_n_j_arrow_i);

			// n_i_arrow_j
			INTTYPE_REST new_n_i_arrow_j = getIteratorValueForNumList(c1Count->n_i_arrow_j, j) + getIteratorValueForNumList(c2Count->n_i_arrow_j, j) +
				c1Count->n_i * c2->countingVars->getIteratorValue(j)->n_i;
			addToNumList(ourCount, i_arrow_j, j, new_n_i_arrow_j);

			if (ourCount->num != j)
			{
				// i != j
				INTTYPE_REST addThis;

				// E start
				// n_i_circ_arrow_square
				addThis = getIteratorValueForNumList(c1Count->n_i_j, j) /* ij */ * (c2->n_circ - c2Count->n_i - c2->countingVars->getIteratorValue(j)->n_i /* j */);
				if (addThis < 0)
				{
				  Rcpp::warning("Warning #15");
				}
				ourCount->n_i_circ_arrow_square += addThis;

				addThis = (c1->n_circ - c1Count->n_i - c1->countingVars->getIteratorValue(j)->n_i /* j */) * getIteratorValueForNumList(c2Count->n_i_j, j) /* ij */;
				if (addThis < 0)
				{
					Rcpp::warning("Warning #17");
				}
				ourCount->n_bracket_i_circ_square += addThis;
				// E end

				// Added by Sand et al. (2014) for filling out tables
				ourCount->n_bracket_i_paren_circ_circ += c1->countingVars->getIteratorValue(j)->n_i * getIteratorValueForNumList(c2Count->n_j_arrow_i, j);
				ourCount->n_bracket_i_paren_circ_square += c1->countingVars->getIteratorValue(j)->n_i * (c2Count->n_circ_arrow_i - getIteratorValueForNumList(c2Count->n_j_arrow_i, j));
				ourCount->n_i_arrow_circ_arrow_circ += getIteratorValueForNumList(c1Count->n_i_arrow_j, j) * c2->countingVars->getIteratorValue(j)->n_i;
				ourCount->n_i_arrow_circ_arrow_square += getIteratorValueForNumList(c1Count->n_i_arrow_j, j) * (c2->n_circ - c2Count->n_i - c2->countingVars->getIteratorValue(j)->n_i);
			}
			lastJPlus1 = j+1;
		}

		ourCount->n_0_arrow_i_circ = c1Count->n_0_arrow_i_circ + c2Count->n_0_arrow_i_circ +
			c1Zero * c2Count->n_i_circ;

		// Added by Sand et al. (2014) for filling out tables
		ourCount->n_paren_i_paren_circ_circ = c1Count->n_paren_i_paren_circ_circ + c2Count->n_paren_i_paren_circ_circ;

		ourCount->n_paren_i_paren_circ_square = c1Count->n_paren_i_paren_circ_square + c2Count->n_paren_i_paren_circ_square;

		ourCount->n_paren_circ_square_arrow_i = c1Count->n_paren_circ_square_arrow_i + c2Count->n_paren_circ_square_arrow_i +
			(c1->n_paren_circ_square - c1Count->n_paren_i_circ) * c2Count->n_i;

		// New counters for calculating E
		ourCount->n_i_circ_square = c1Count->n_i_circ_square + c2Count->n_i_circ_square;

		ourCount->n_circ_square_arrow_i = c1Count->n_circ_square_arrow_i + c2Count->n_circ_square_arrow_i +
			(c1->n_circ_square - c1Count->n_i_circ) * c2Count->n_i;

		ourCount->n_paren_i_circ_square = c1Count->n_paren_i_circ_square + c2Count->n_paren_i_circ_square;

		ourCount->n_0_i_circ = c1Count->n_0_i_circ + c2Count->n_0_i_circ;

		ourCount->n_i_circ_arrow_0 = c1Count->n_i_circ_arrow_0 + c2Count->n_i_circ_arrow_0 +
			c1Count->n_i_circ * c2Zero;

		ourCount->n_0_i_arrow_circ = c1Count->n_0_i_arrow_circ + c2Count->n_0_i_arrow_circ +
			c1Count->n_0_i * (c2->n_circ - c2Count->n_i);

		ourCount->n_0_circ_arrow_i = c1Count->n_0_circ_arrow_i + c2Count->n_0_circ_arrow_i +
			(c1->n_0_circ - c1Count->n_0_i) * c2Count->n_i;

		ourCount->n_paren_0_i_circ = c1Count->n_paren_0_i_circ + c2Count->n_paren_0_i_circ;

		ourCount->n_bracket_0_i_circ = c1Count->n_bracket_0_i_circ + c2Count->n_bracket_0_i_circ +
			c1Zero * c2Count->n_i_circ + 
			c1Count->n_i * (c2->n_0_circ - c2Count->n_0_i) +
			(c1->n_circ - c1Count->n_i) * c2Count->n_0_i;

		// Sums and stuff
		n_circ_square += ourCount->n_i_circ;
				
		// Quartets
		n_circ_circ += ourCount->n_ii;
		n_square_paren_circ_circ += ourCount->n_i_paren_circ_circ;
		n_paren_circ_circ += ourCount->n_paren_ii;
		n_paren_circ_square += ourCount->n_paren_i_circ;
		n_circ_arrow_paren_square_square += ourCount->n_circ_arrow_paren_ii; // FIXED!!
		n_bracket_circ_square += ourCount->n_bracket_i_circ;

		// Added by Sand et al. (2014)
		n_circ_arrow_square_square += ourCount->n_i_arrow_circ_circ;

		// Added by Sand et al. (2014) for filling out tables
		// A
		n_bracket_circ_circ += Util::binom2(ourCount->n_i);
		n_paren_circ_paren_square_square += ourCount->n_paren_i_paren_circ_circ;
		n_circ_arrow_circ += ourCount->n_i_arrow_i;
		n_circ_arrow_square += ourCount->n_i_arrow_circ;
		n_paren_circ_circ_arrow_square += ourCount->n_paren_ii_arrow_circ;
		n_bracket_circ_paren_square_square += ourCount->n_bracket_i_paren_circ_circ;
		n_circ_arrow_square_arrow_square += ourCount->n_circ_arrow_i_arrow_i;

		// New sums and stuff for calculating E
		n_circ_square_arrow_triangle += ourCount->n_circ_square_arrow_i;
		n_circ_arrow_square_triangle += ourCount->n_i_arrow_circ_square;
		
		n_0_circ_arrow_square += ourCount->n_0_i_arrow_circ;
		n_0_arrow_circ_square += ourCount->n_0_arrow_i_circ;
		n_circ_square_arrow_0 += ourCount->n_i_circ_arrow_0;
		n_circ_arrow_0_square += ourCount->n_i_arrow_0_circ;

		n_bracket_circ_square_triangle += ourCount->n_bracket_i_circ_square;
		n_bracket_0_circ_square += ourCount->n_bracket_0_i_circ;

		// Update resolved/resolved and unresolved/unresolved
		// (Figure 10)
		tripResolved += c1Count->n_i * c2Count->n_i_arrow_circ;
		tripResolved += Util::binom2(c1Count->n_i) * (c2->n_circ - c2Count->n_i);
		tripResolved += (c1->n_circ - c1Count->n_i) * c2Count->n_paren_ii;
		tripUnresolved += c1Count->n_i * (c2->n_circ_square - c2Count->n_i_circ);

		// Quartets
		//quartResolvedAgree & quartResolvedDisagree

		// alpha & alpha
		quartResolvedAgreeDiag += (INTTYPE_N4) c1Count->n_i * c2Count->n_i_arrow_paren_circ_circ;
		quartResolvedAgreeDiag += (INTTYPE_N4) Util::binom2(c1Count->n_i) * (c2->n_paren_circ_circ - c2Count->n_paren_ii);

		// beta & alpha
		quartResolvedAgree += (INTTYPE_N4) c1Count->n_i * c2Count->n_i_arrow_paren_circ_square;
		quartResolvedAgree += (INTTYPE_N4) Util::binom2(c1Count->n_i) * (c2->n_paren_circ_square - c2Count->n_paren_i_circ);
		quartResolvedAgree += (INTTYPE_N4) (c1->n_bracket_circ_square - c1Count->n_bracket_i_circ) * c2Count->n_paren_ii;
		quartResolvedAgree += (INTTYPE_N4) c1Count->n_i * (c2->n_circ_arrow_paren_square_square - c2Count->n_i_arrow_paren_circ_circ - c2Count->n_circ_arrow_paren_ii);
		
		// beta & beta (part 1)
		quartResolvedAgreeDiag += (INTTYPE_N4) c1Count->n_i * c2Count->n_i_arrow_circ_square;
		quartResolvedAgreeDiag += (INTTYPE_N4) Util::binom2(c1Count->n_i) * (c2->n_circ_square - c2Count->n_i_circ);
		quartResolvedAgreeDiag += (INTTYPE_N4) c1Count->n_i * (c2->n_square_paren_circ_circ - c2Count->n_i_paren_circ_circ - c2Count->n_circ_paren_ii);
		
		// beta & beta (part 2)
		quartResolvedAgreeDiag += (INTTYPE_N4) c1Count->n_i * (c2->n_circ_arrow_square_square - c2Count->n_i_arrow_circ_circ - c2Count->n_circ_arrow_ii);
		quartResolvedAgreeDiag += (INTTYPE_N4) (c1->n_bracket_circ_square - c1Count->n_bracket_i_circ) * c2Count->n_ii;
		quartResolvedAgreeDiag += (INTTYPE_N4) c1Count->n_i * c2Count->n_i_paren_circ_square;
		
		// gamma & alpha
		quartResolvedAgree += (INTTYPE_N4) c1Count->n_i * c2Count->n_i_arrow_paren_0_circ;
		quartResolvedAgree += (INTTYPE_N4) Util::binom2(c1Count->n_i) * (c2->n_paren_0_circ - c2Count->n_paren_0_i);
		quartResolvedAgree += (INTTYPE_N4) c1Zero * (c1->n_circ - c1Count->n_i) * c2Count->n_paren_ii;
		quartResolvedAgree += (INTTYPE_N4) (c1->n_circ - c1Count->n_i) * c2Count->n_0_arrow_paren_ii;
		quartResolvedAgree += (INTTYPE_N4) c1Zero * c2Count->n_circ_arrow_paren_ii;
		
		// gamma & beta (part 1)
		quartResolvedAgree += (INTTYPE_N4) Util::binom2(c1Count->n_i) * (c2->n_0_circ - c2Count->n_0_i);
		quartResolvedAgree += (INTTYPE_N4) c1Count->n_i * c2Count->n_i_arrow_0_circ;
		quartResolvedAgree += (INTTYPE_N4) c1Zero * c2Count->n_circ_paren_ii;
		quartResolvedAgree += (INTTYPE_N4) (c1->n_circ - c1Count->n_i) * c2Count->n_0_paren_ii;
		
		// gamma & beta (part 2)
		quartResolvedAgree += (INTTYPE_N4) (c1->n_circ - c1Count->n_i) * c2Count->n_0_arrow_ii;
		quartResolvedAgree += (INTTYPE_N4) c1Zero * c2Count->n_circ_arrow_ii;
		quartResolvedAgree += (INTTYPE_N4) c1Zero * (c1->n_circ - c1Count->n_i) * c2Count->n_ii;
		quartResolvedAgree += (INTTYPE_N4) c1Count->n_i * c2Count->n_i_paren_0_circ;
		
		// gamma & gamma (part 1)
		quartResolvedAgreeDiag += (INTTYPE_N4) c1Count->n_i * c2Count->n_i_arrow_circ_arrow_0;
		quartResolvedAgreeDiag += (INTTYPE_N4) Util::binom2(c1Count->n_i) * (c2->n_circ_arrow_0 - c2Count->n_i_arrow_0);
		quartResolvedAgreeDiag += (INTTYPE_N4) (c1->n_circ - c1Count->n_i) * c2Count->n_paren_ii_arrow_0;
		quartResolvedAgreeDiag += (INTTYPE_N4) c1Count->n_bracket_circ_paren_ii * c2Zero;
		quartResolvedAgreeDiag += (INTTYPE_N4) c1Zero * c2Count->n_paren_circ_paren_ii;
		
		// gamma & gamma (part 2)
		quartResolvedAgreeDiag += (INTTYPE_N4) c1Count->n_i * c2Count->n_i_arrow_0_arrow_circ;
		quartResolvedAgreeDiag += (INTTYPE_N4) Util::binom2(c1Count->n_i) * (c2->n_0_arrow_circ - c2Count->n_0_arrow_i);
		quartResolvedAgreeDiag += (INTTYPE_N4) c1Zero * c2Count->n_paren_ii_arrow_circ;
		quartResolvedAgreeDiag += (INTTYPE_N4) c1Count->n_bracket_0_paren_ii * (c2->n_circ - c2Count->n_i);
		quartResolvedAgreeDiag += (INTTYPE_N4) (c1->n_circ - c1Count->n_i) * c2Count->n_paren_0_paren_ii;
		
		// gamma & gamma (part 3) (aka the exiting conclusion)
		quartResolvedAgreeDiag += (INTTYPE_N4) c1Zero * c2Count->n_circ_arrow_i_arrow_i;
		quartResolvedAgreeDiag += (INTTYPE_N4) (c1->n_circ - c1Count->n_i) * c2Count->n_0_arrow_i_arrow_i;
		quartResolvedAgreeDiag += (INTTYPE_N4) c1Zero * (c1->n_circ - c1Count->n_i) * c2Count->n_i_arrow_i;
		quartResolvedAgreeDiag += (INTTYPE_N4) c1Count->n_i * c2Count->n_paren_0_circ_arrow_i;
		quartResolvedAgreeDiag += (INTTYPE_N4) c1Count->n_bracket_i_paren_0_circ * c2Count->n_i;
		quartResolvedAgreeDiag += (INTTYPE_N4) c1Count->n_i * c2Count->n_paren_i_paren_0_circ;

		//
		// +------------------------------------------+
		// | New sums for calculating A (Added by Sand et al. (2014)) |
		// +------------------------------------------+
		//

		// alpha & beta
		quartResolvedAgreeUpper += (INTTYPE_N4) c1Count->n_i * c2Count->n_i_arrow_circ_circ;
		quartResolvedAgreeUpper += (INTTYPE_N4) Util::binom2(c1Count->n_i) * (c2->n_circ_circ - c2Count->n_ii);
		quartResolvedAgreeUpper += (INTTYPE_N4) c1Count->n_i * c2Count->n_i_paren_circ_circ;

		// alpha & gamma
		quartResolvedAgreeUpper += (INTTYPE_N4) c1Count->n_i * c2Count->n_i_arrow_circ_arrow_circ;
		quartResolvedAgreeUpper += (INTTYPE_N4) Util::binom2(c1Count->n_i) * (c2->n_circ_arrow_circ - c2Count->n_i_arrow_i);
		quartResolvedAgreeUpper += (INTTYPE_N4) c1Count->n_i * c2Count->n_paren_circ_circ_arrow_i;
		quartResolvedAgreeUpper += (INTTYPE_N4) c1Count->n_bracket_i_paren_circ_circ * c2Count->n_i;
		quartResolvedAgreeUpper += (INTTYPE_N4) c1Count->n_i * c2Count->n_paren_i_paren_circ_circ;

		// beta & gamma (part 1)
		quartResolvedAgreeUpper += (INTTYPE_N4) c1Count->n_i * c2Count->n_i_arrow_circ_arrow_square;
		quartResolvedAgreeUpper += (INTTYPE_N4) Util::binom2(c1Count->n_i) * (c2->n_circ_arrow_square - c2Count->n_i_arrow_circ - c2Count->n_circ_arrow_i);
		quartResolvedAgreeUpper += (INTTYPE_N4) c1Count->n_i * (c2->n_paren_circ_circ_arrow_square - c2Count->n_paren_ii_arrow_circ - c2Count->n_paren_circ_circ_arrow_i);
		quartResolvedAgreeUpper += (INTTYPE_N4) (c1->n_bracket_circ_paren_square_square - c1Count->n_bracket_i_paren_circ_circ - c1Count->n_bracket_circ_paren_ii) * c2Count->n_i;
		quartResolvedAgreeUpper += (INTTYPE_N4) c1Count->n_i * (c2->n_paren_circ_paren_square_square - c2Count->n_paren_i_paren_circ_circ - c2Count->n_paren_circ_paren_ii);

		// beta & gamma (part 2)
		quartResolvedAgreeUpper += (INTTYPE_N4) c1Count->n_i * (c2->n_circ_arrow_square_arrow_square - c2Count->n_i_arrow_circ_arrow_circ - c2Count->n_circ_arrow_i_arrow_i);
		quartResolvedAgreeUpper += (INTTYPE_N4) (c1->n_bracket_circ_square - c1Count->n_bracket_i_circ) * c2Count->n_i_arrow_i;
		quartResolvedAgreeUpper += (INTTYPE_N4) c1Count->n_i * c2Count->n_paren_circ_square_arrow_i;
		quartResolvedAgreeUpper += (INTTYPE_N4) c1Count->n_bracket_i_paren_circ_square * c2Count->n_i;
		quartResolvedAgreeUpper += (INTTYPE_N4) c1Count->n_i * c2Count->n_paren_i_paren_circ_square;

		//
		// +------------------------------------------+
		// | New sums for calculating E (Added by Sand et al. (2014)) |
		// +------------------------------------------+
		//

		// delta & delta
		quartSumE += (INTTYPE_N4) c1Count->n_i * (c2->n_circ_square_triangle - c2Count->n_i_circ_square);

		// delta & epsilon
		quartSumE += (INTTYPE_N4) c1Count->n_i * (c2->n_circ_square_arrow_triangle - c2Count->n_i_circ_arrow_square - c2Count->n_circ_square_arrow_i);
		quartSumE += (INTTYPE_N4) (c1->n_bracket_circ_square_triangle - c1Count->n_bracket_i_circ_square) * c2Count->n_i;
		quartSumE += (INTTYPE_N4) c1Count->n_i * (c2->n_paren_circ_square_triangle - c2Count->n_paren_i_circ_square);
		
		// epsilon & delta (countinues below)
		quartSumE += (INTTYPE_N4) c1Count->n_i * (c2->n_0_circ_square - c2Count->n_0_i_circ);

		// epsilon & epsilon (part 1) (continues below)
		quartSumE += (INTTYPE_N4) c1Count->n_i * (c2->n_circ_square_arrow_0 - c2Count->n_i_circ_arrow_0);

		// epsilon & epsilon (part 2) (continues below)
		quartSumE += (INTTYPE_N4) c1Count->n_i * (c2->n_0_circ_arrow_square - c2Count->n_0_i_arrow_circ - c2Count->n_0_circ_arrow_i);
		quartSumE += (INTTYPE_N4) (c1->n_bracket_0_circ_square - c1Count->n_bracket_0_i_circ) * c2Count->n_i;
		quartSumE += (INTTYPE_N4) c1Count->n_i * (c2->n_paren_0_circ_square - c2Count->n_paren_0_i_circ);

		// Go to next on children unless we're done
		if (c1Next == NULL && c2Next == NULL)
			ourCount->type = CountingLinkedList::End;
		else
		{
			// Go to next one (there's more!)
			ourCount->type = CountingLinkedList::Regular;
			if (ourCount->next == NULL) ourCount->next = factory->getLL();
			ourCount = ourCount->next;
		}
	}
	n_circ_square /= 2;

	// Quartets
	n_paren_circ_square /= 2;
	n_bracket_circ_square /= 2;

	// New sums for calculating E
	// epsilon & delta (continued from inside the loop)
	quartSumE += (INTTYPE_N4) c1Zero * c2->n_circ_square_triangle;

	// epsilon & epsilon (part 1) (continued from inside the loop)
	quartSumE += (INTTYPE_N4) c1->n_bracket_circ_square_triangle * c2Zero;
	quartSumE += (INTTYPE_N4) c1Zero * c2->n_paren_circ_square_triangle;

	// epsilon & epsilon (part 2) (continued from inside the loop)
	quartSumE += (INTTYPE_N4) c1Zero * c2->n_circ_square_arrow_triangle;

	// Div E sum-counters
	if (n_bracket_circ_square_triangle % 3 != 0)
	{
		Rcpp::warning("n_bracket_circ_square_triangle mod 3 check failed");
	}
	n_bracket_circ_square_triangle /= 3;

	if (n_bracket_0_circ_square % 2 != 0)
	{
	  Rcpp::warning("n_bracket_0_circ_square mod 2 check failed");
	}
	n_bracket_0_circ_square /= 2;

	if (n_circ_square_arrow_0 % 2 != 0)
	{
	  Rcpp::warning("n_circ_square_arrow_0 mod 2 check failed");
	}
	n_circ_square_arrow_0 /= 2;

	if (n_0_arrow_circ_square % 2 != 0)
	{
	  Rcpp::warning("n_0_arrow_circ_square mod 2 check failed");
	}
	n_0_arrow_circ_square /= 2;
}
