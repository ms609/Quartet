#include "hdt.h"
#include "rooted_tree.h"
#include "hdt_factory.h"
#include "counting_linked_list.h"

void HDT::handleLeaf()
{
	// This is a leaf!
	// Triplets
	if (link == NULL)
	{
		countingVars->num = 0;
		countingVars->n_i = numZeroes;
	}
	else
	{
		countingVars->num = this->link->color;
		countingVars->n_i = 1;
	}
	countingVars->n_i_circ = 0;
	countingVars->n_paren_ii = 0;
	countingVars->n_i_arrow_circ = 0;

	// Quartets
	countingVars->n_0_i = 0;
	countingVars->n_ii = 0;
	countingVars->n_0_paren_ii = 0;
	countingVars->n_circ_paren_ii = 0;
	countingVars->n_i_paren_0_circ = 0;
	countingVars->n_i_paren_circ_circ = 0;
	countingVars->n_i_paren_circ_square = 0;

	countingVars->n_paren_0_i = 0;
	countingVars->n_paren_i_circ = 0;
	countingVars->n_paren_0_paren_ii = 0;
	countingVars->n_paren_circ_paren_ii = 0;
	countingVars->n_paren_i_paren_0_circ = 0;

	countingVars->n_bracket_0_paren_ii = 0;
	countingVars->n_bracket_circ_paren_ii = 0;
	countingVars->n_bracket_i_paren_0_circ = 0;
	countingVars->n_bracket_i_circ = 0;

	countingVars->n_0_arrow_i = 0;
	countingVars->n_i_arrow_0 = 0;
	countingVars->n_i_arrow_i = 0;
	countingVars->n_circ_arrow_i = 0;
	countingVars->n_0_arrow_paren_ii = 0;
	countingVars->n_i_arrow_paren_0_circ = 0;
	countingVars->n_i_arrow_paren_circ_square = 0;
	countingVars->n_circ_arrow_paren_ii = 0;
	countingVars->n_i_arrow_0_circ = 0;
	countingVars->n_i_arrow_circ_circ = 0;
	countingVars->n_i_arrow_circ_square = 0;
	countingVars->n_circ_arrow_ii = 0;
	countingVars->n_paren_ii_arrow_0 = 0;
	countingVars->n_paren_ii_arrow_circ = 0;
	countingVars->n_paren_circ_circ_arrow_i = 0;
	countingVars->n_0_arrow_i_arrow_i = 0;
	countingVars->n_i_arrow_circ_arrow_0 = 0;
	countingVars->n_i_arrow_0_arrow_circ = 0;
	countingVars->n_circ_arrow_i_arrow_i = 0;
	
	// Added by Sand et al. (2014)
	countingVars->n_i_arrow_paren_circ_circ = 0;
	countingVars->n_0_arrow_ii = 0;
	countingVars->n_paren_0_circ_arrow_i = 0;

	//countingVars->n_i_j = NULL; // it already is!
	countingVars->n_0_arrow_i_circ = 0;

	// Added by Sand et al. (2014) for filling out tables
	// A
	countingVars->n_paren_i_paren_circ_circ = 0;
	countingVars->n_bracket_i_paren_circ_circ = 0;
	countingVars->n_paren_i_paren_circ_square = 0;
	countingVars->n_bracket_i_paren_circ_square = 0;
	countingVars->n_i_arrow_circ_arrow_circ = 0;
	countingVars->n_i_arrow_circ_arrow_square = 0;
	countingVars->n_paren_circ_square_arrow_i = 0;

	// New counters for calculating E
	countingVars->n_i_circ_square = 0;
	countingVars->n_i_circ_arrow_square = 0;
	countingVars->n_circ_square_arrow_i = 0;
	countingVars->n_paren_i_circ_square = 0;
	countingVars->n_0_i_circ = 0;
	countingVars->n_i_circ_arrow_0 = 0;
	countingVars->n_0_i_arrow_circ = 0;
	countingVars->n_0_circ_arrow_i = 0;
	countingVars->n_paren_0_i_circ = 0;
	countingVars->n_bracket_i_circ_square = 0;
	countingVars->n_bracket_0_i_circ = 0;

	countingVars->type = CountingLinkedList::End;

	// Sums and stuff (triplets)
	if (countingVars->num != 0) n_circ = 1;
	else n_circ = 0;
	n_circ_square = 0;

	// Sums (quartets)
	n_0_circ = 0;
	n_circ_circ = 0;
	n_square_paren_circ_circ = 0;
	n_paren_circ_circ = 0;
	n_paren_circ_square = 0;
	n_circ_arrow_paren_square_square = 0;
	n_bracket_circ_square = 0;
	n_paren_0_circ = 0;

	n_0_arrow_circ = 0;
	n_circ_arrow_0 = 0;
	n_0_arrow_circ_circ = 0;

	// Added by Sand et al. (2014)
	n_circ_arrow_square_square = 0;

	// Added by Sand et al. (2014) for filling out tables
	n_bracket_circ_circ = 0;
	n_paren_circ_paren_square_square = 0;
	n_circ_arrow_circ = 0;
	n_circ_arrow_square = 0;
	n_paren_circ_circ_arrow_square = 0;
	n_bracket_circ_paren_square_square = 0;
	n_circ_arrow_square_arrow_square = 0;

	// New sums for calculating E
	n_circ_square_triangle = 0;
	n_circ_square_arrow_triangle = 0;
	n_circ_arrow_square_triangle = 0;
	n_paren_circ_square_triangle = 0;
	n_0_circ_square = 0;
	n_0_circ_arrow_square = 0;
	n_0_arrow_circ_square = 0;
	n_circ_square_arrow_0 = 0;
	n_circ_arrow_0_square = 0;
	n_paren_0_circ_square = 0;
	n_bracket_circ_square_triangle = 0;
	n_bracket_0_circ_square = 0;
}
