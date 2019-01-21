#include <Rcpp.h>
#include "hdt.h"
#include "rooted_tree.h"
#include "hdt_factory.h"
#include "counting_linked_list.h"

void HDT::handleCTransform()
{
	// Triplets (sum, n_i_circ has been set to 0, i.e. n_circ_squar is also 0!)
	n_circ_square = 0;

	// Quartets
	// Sums initialized to 0, summed in loop below
	n_paren_circ_circ = 0;
	n_paren_circ_square = 0;
	
	// Not dependent on i, thus placed here. Value is 0 (always! (when transforming C to G))
	n_0_circ = 0;

	// Get the number of leafs with color 0
	// The list is sorted, i.e. if zero's there it's the first one!
	INTTYPE_REST gZero = 0;
	if (countingVars->num == 0) gZero = countingVars->n_i;

	// Not dependent on i (n_circ has been updated in handleC!)
	n_paren_0_circ = gZero * n_circ;


	// Added by Sand et al. (2014) for filling out tables (initialize)
	n_paren_circ_paren_square_square = 0;

	// Reset sums and stuff for calculating E
	n_circ_square_triangle = 0;
	n_0_circ_square = 0;
	n_paren_circ_square_triangle = 0;
	n_paren_0_circ_square = 0;

	CountingLinkedList *current = countingVars;
	if (current->num == 0)
	{
		if (current->type == CountingLinkedList::End) current = NULL;
		else current = current->next; // don't do weird stuff for the n_0 case...
	}

	while(current != NULL)
	{
		//current->n_i is unchanged by this transform!
		// Triplets
		current->n_i_circ = 0;
		current->n_paren_ii = Util::binom2(current->n_i);

		// Quartets
		// 2nd group in figure 12 (quartets only)
		current->n_0_i = 0;
		current->n_ii = 0;
		current->n_0_paren_ii = 0;
		current->n_circ_paren_ii = 0;
		current->n_i_paren_0_circ = 0;
		current->n_i_paren_circ_circ = 0;
		current->n_i_paren_circ_square = 0;

		// 3nd group in figure 12 (quartets only)
		current->n_paren_0_i = gZero * current->n_i;
		current->n_paren_i_circ = current->n_bracket_i_circ; // Wee, we could spare a calculation here =)
		current->n_paren_0_paren_ii = current->n_bracket_0_paren_ii;
		current->n_paren_circ_paren_ii = current->n_bracket_circ_paren_ii;
		current->n_paren_i_paren_0_circ = current->n_bracket_i_paren_0_circ;

		// Count up the sums
		n_paren_circ_circ += current->n_paren_ii;
		n_paren_circ_square += current->n_paren_i_circ;

		// Figure 15 counters (part 1)
		if (current->n_i_j != NULL) current->n_i_j->type = CountingLinkedListNumOnly::SkipAndEnd;

		// Added by Sand et al. (2014) for filling out tables
		// A
		current->n_paren_i_paren_circ_circ = current->n_bracket_i_paren_circ_circ;
		current->n_paren_i_paren_circ_square = current->n_bracket_i_paren_circ_square;

		// Added by Sand et al. (2014) for filling out tables (sum)
		// A
		n_paren_circ_paren_square_square += current->n_paren_i_paren_circ_circ;
		
		// New counters for calculating E
		current->n_i_circ_square = 0;
		current->n_0_i_circ = 0;
		current->n_paren_i_circ_square = current->n_bracket_i_circ_square;
		current->n_paren_0_i_circ = current->n_bracket_0_i_circ;
		
		// New sums for E
		n_paren_circ_square_triangle += current->n_paren_i_circ_square;
		n_paren_0_circ_square += current->n_paren_0_i_circ;

		// Go to next on children unless we're done
		if (current->type != CountingLinkedList::End)
		{
			// Go to next one (there's more!)
			current = current->next;
		}
		else current = NULL;
	}

	// These are all based on stuff that is reset to 0
	n_circ_circ = 0;
	n_square_paren_circ_circ = 0;

	// Halve it :)
	n_paren_circ_square /= 2;

	// Take a third of some of the E sums
	if (n_paren_circ_square_triangle % 3 != 0)
	{
	  Rcpp::stop("n_paren_circ_square_triangle mod 3 test... FAIL!!!");
	}
	n_paren_circ_square_triangle /= 3;

	// Halve some other ones
	if (n_paren_0_circ_square % 2 != 0)
	{
	  Rcpp::stop("n_paren_0_circ_square mod 2 test... FAIL!!!");
	}
	n_paren_0_circ_square /= 2;
}
