#include "hdt.h"
#include "rooted_tree.h"
#include "hdt_factory.h"
#include "counting_linked_list.h"

void HDT::handleIGToC()
{
	// NOTE: We generate this with left being I and right being G!

	if (!right->up2date) right->updateCounters();

	// Fetch the childrens "result counting data"
	tripResolved = right->tripResolved;
	tripUnresolved = right->tripUnresolved;

	// These are just inherited from the G node
	n_circ = right->n_circ;
	n_circ_square = right->n_circ_square;

	// Quartets
	// Fetch the childrens "result counting data"
	quartResolvedAgree = right->quartResolvedAgree;
	quartResolvedAgreeDiag = right->quartResolvedAgreeDiag;
	quartResolvedAgreeUpper = right->quartResolvedAgreeUpper;

	// These sums are just inherited from the G node
	n_circ_circ = right->n_circ_circ;
	n_square_paren_circ_circ = right->n_square_paren_circ_circ;
	n_paren_circ_circ = right->n_paren_circ_circ;
	n_paren_circ_square = right->n_paren_circ_square;
	n_bracket_circ_square = right->n_bracket_circ_square;

	// Sums
	// Not actually defined in G! IN G!!! (i.e. it *should* in fact be 0)
	n_circ_arrow_paren_square_square = 0;
	n_circ_arrow_square_square = 0; // Added by Sand et al. (2014)

	// Not dependent on i
	n_0_circ = right->n_0_circ;
	n_paren_0_circ = right->n_paren_0_circ;

	// 5th group, not dependent on i
	// Not actually defined in G! IN G!!! (i.e. it *should* in fact be 0)
	n_0_arrow_circ = 0;
	n_circ_arrow_0 = 0;
	n_0_arrow_circ_circ = 0;
		
	// Added by Sand et al. (2014) for filling out tables
	n_bracket_circ_circ = right->n_bracket_circ_circ;
	n_paren_circ_paren_square_square = right->n_paren_circ_paren_square_square;
	n_circ_arrow_circ = 0;
	n_circ_arrow_square = 0;
	n_paren_circ_circ_arrow_square = 0;
	n_bracket_circ_paren_square_square = right->n_bracket_circ_paren_square_square;
	n_circ_arrow_square_arrow_square = 0;

	// Initialize sums and stuff for calculating E
	n_circ_square_triangle = right->n_circ_square_triangle;
	n_circ_square_arrow_triangle = 0;
	n_circ_arrow_square_triangle = 0;
	n_paren_circ_square_triangle = right->n_paren_circ_square_triangle;
	n_0_circ_square = right->n_0_circ_square;
	n_0_circ_arrow_square = 0;
	n_0_arrow_circ_square = 0;
	n_circ_square_arrow_0 = 0;
	n_circ_arrow_0_square = 0;
	n_paren_0_circ_square = right->n_paren_0_circ_square;
	n_bracket_circ_square_triangle = right->n_bracket_circ_square_triangle;
	n_bracket_0_circ_square = right->n_bracket_0_circ_square;

	// Fetch sum for calculating E from children
	quartSumE = right->quartSumE;

	CountingLinkedList *current = right->countingVars;
	CountingLinkedList *ourCount = countingVars;
	while(current != NULL)
	{
		ourCount->num = current->num;
		ourCount->type = current->type;

		// Triplets
		ourCount->n_i = current->n_i;
		if (ourCount->num == 0)
		{
			// Go to next one => We're done!
			if (ourCount->type != CountingLinkedList::End)
			{
				// Go to next one (there's more!)
				if (ourCount->next == NULL) ourCount->next = factory->getLL();
				ourCount = ourCount->next;

				current = current->next;
			}
			else current = NULL;

			continue;
		}

		ourCount->n_i_circ = current->n_i_circ;
		ourCount->n_paren_ii = current->n_paren_ii;
		ourCount->n_i_arrow_circ = 0; // Not actually defined in G! IN G!!! (i.e. it *should* in fact be 0)

		// Quartets
		// 2nd group in figure 12 (quartets only)
		ourCount->n_0_i = current->n_0_i;
		ourCount->n_ii = current->n_ii;
		ourCount->n_0_paren_ii = current->n_0_paren_ii;
		ourCount->n_circ_paren_ii = current->n_circ_paren_ii;
		ourCount->n_i_paren_0_circ = current->n_i_paren_0_circ;
		ourCount->n_i_paren_circ_circ = current->n_i_paren_circ_circ;
		ourCount->n_i_paren_circ_square = current->n_i_paren_circ_square;

		// In the box (based on calculations that are just inherited why we can just inherit the value)
		ourCount->n_bracket_i_circ = current->n_bracket_i_circ;

		// 3rd group in figure 12 (quartets only)
		ourCount->n_paren_0_i = current->n_paren_0_i;
		ourCount->n_paren_i_circ = current->n_paren_i_circ;
		ourCount->n_paren_0_paren_ii = current->n_paren_0_paren_ii;
		ourCount->n_paren_circ_paren_ii = current->n_paren_circ_paren_ii;
		ourCount->n_paren_i_paren_0_circ = current->n_paren_i_paren_0_circ;

		// 4th group in figure 12 (quartets only)
		ourCount->n_bracket_0_paren_ii = current->n_bracket_0_paren_ii;
		ourCount->n_bracket_circ_paren_ii = current->n_bracket_circ_paren_ii;
		ourCount->n_bracket_i_paren_0_circ = current->n_bracket_i_paren_0_circ;

		// 5th group in figure 12 (quartets only)
		ourCount->n_0_arrow_i = 0;
		ourCount->n_i_arrow_0 = 0;
		ourCount->n_i_arrow_i = 0;
		ourCount->n_circ_arrow_i = 0;
		ourCount->n_0_arrow_paren_ii = 0;
		ourCount->n_i_arrow_paren_0_circ = 0;
		ourCount->n_i_arrow_paren_circ_square = 0;
		ourCount->n_circ_arrow_paren_ii = 0;
		ourCount->n_i_arrow_0_circ = 0;
		ourCount->n_i_arrow_circ_circ = 0;
		ourCount->n_i_arrow_circ_square = 0;
		ourCount->n_circ_arrow_ii = 0;
		ourCount->n_paren_ii_arrow_0 = 0;
		ourCount->n_paren_ii_arrow_circ = 0;
		ourCount->n_paren_circ_circ_arrow_i = 0;
		ourCount->n_0_arrow_i_arrow_i = 0;
		ourCount->n_i_arrow_circ_arrow_0 = 0;
		ourCount->n_i_arrow_0_arrow_circ = 0;
		ourCount->n_circ_arrow_i_arrow_i = 0;

		// Added by Sand et al. (2014)
		ourCount->n_i_arrow_paren_circ_circ = 0;
		ourCount->n_0_arrow_ii = 0;
		ourCount->n_paren_0_circ_arrow_i = 0;
		
		// Figure 15 counters (part 1+2, all with j)
		ourCount->resetIterator();
		current->resetIterator();
		unsigned int wantedMax = degree+1;
		unsigned int lastJPlus1 = 1;
		if (ourCount->n_i_j != NULL) ourCount->n_i_j->type = CountingLinkedListNumOnly::SkipAndEnd;

		while (true)
		{
			unsigned int j = wantedMax;
			NEXT_LEAST_J(current->n_i_j)
			if (j >= wantedMax) break;

			// n_i_j
			addToNumList(ourCount, i_j, j, getIteratorValueForNumList(current->n_i_j, j));
			lastJPlus1 = j+1;
		}

		ourCount->n_0_arrow_i_circ = 0;

		
		// Added by Sand et al. (2014) for filling out tables
		// A
		ourCount->n_paren_i_paren_circ_circ = current->n_paren_i_paren_circ_circ;
		ourCount->n_bracket_i_paren_circ_circ = current->n_bracket_i_paren_circ_circ;
		if (ourCount->n_j_arrow_i != NULL) ourCount->n_j_arrow_i->type = CountingLinkedListNumOnly::SkipAndEnd;
		ourCount->n_paren_i_paren_circ_square = current->n_paren_i_paren_circ_square;
		ourCount->n_bracket_i_paren_circ_square = current->n_bracket_i_paren_circ_square;
		ourCount->n_i_arrow_circ_arrow_circ = 0;
		ourCount->n_i_arrow_circ_arrow_square = 0;
		ourCount->n_paren_circ_square_arrow_i = 0;
		if (ourCount->n_i_arrow_j != NULL) ourCount->n_i_arrow_j->type = CountingLinkedListNumOnly::SkipAndEnd;

		// New counters for calculating E
		ourCount->n_i_circ_square = current->n_i_circ_square;
		ourCount->n_i_circ_arrow_square = 0;
		ourCount->n_circ_square_arrow_i = 0;
		ourCount->n_paren_i_circ_square = current->n_paren_i_circ_square;
		ourCount->n_0_i_circ = current->n_0_i_circ;
		ourCount->n_i_circ_arrow_0 = 0;
		ourCount->n_0_i_arrow_circ = 0;
		ourCount->n_0_circ_arrow_i = 0;
		ourCount->n_paren_0_i_circ = current->n_paren_0_i_circ;
		ourCount->n_bracket_i_circ_square = current->n_bracket_i_circ_square;
		ourCount->n_bracket_0_i_circ = current->n_bracket_0_i_circ;

		// Go to next on children unless we're done
		if (ourCount->type != CountingLinkedList::End)
		{
			// Go to next one (there's more!)
			if (ourCount->next == NULL) ourCount->next = factory->getLL();
			ourCount = ourCount->next;

			current = current->next;
		}
		else current = NULL;
	}
}
