#ifndef COUNTING_LINKED_LIST_H
#define COUNTING_LINKED_LIST_H

#include "int_stuff.h"
#include "counting_linked_list_num_only.h"
#include "hdt_factory.h"

class CountingLinkedList
{
	public:
		// For triplets
		INTTYPE_REST n_i, n_i_circ, n_paren_ii, n_i_arrow_circ;

		// For quartets
#ifdef quartetsToo
		INTTYPE_REST n_0_i, n_ii, n_0_paren_ii, n_circ_paren_ii, n_i_paren_0_circ, n_i_paren_circ_circ, n_i_paren_circ_square;
		INTTYPE_REST n_bracket_i_circ;
		INTTYPE_REST n_paren_0_i, n_paren_i_circ, n_paren_0_paren_ii, n_paren_circ_paren_ii, n_paren_i_paren_0_circ;
		INTTYPE_REST n_bracket_0_paren_ii, n_bracket_circ_paren_ii, n_bracket_i_paren_0_circ;
		INTTYPE_REST n_0_arrow_i, n_i_arrow_0, n_i_arrow_i, n_circ_arrow_i, n_0_arrow_paren_ii, n_i_arrow_paren_0_circ,
			n_i_arrow_paren_circ_square, n_circ_arrow_paren_ii, n_i_arrow_0_circ, n_i_arrow_circ_circ,
			n_i_arrow_circ_square, n_circ_arrow_ii, n_paren_ii_arrow_0, n_paren_ii_arrow_circ, n_paren_circ_circ_arrow_i,
			n_0_arrow_i_arrow_i, n_i_arrow_circ_arrow_0, n_i_arrow_0_arrow_circ, n_circ_arrow_i_arrow_i;

		// Added by us
		INTTYPE_REST n_i_arrow_paren_circ_circ, n_0_arrow_ii, n_paren_0_circ_arrow_i;

		// Figure 15 counters (part 1)
		CountingLinkedListNumOnly *n_i_j; // also used for E calculation
		INTTYPE_REST n_0_arrow_i_circ; // also used for E calculation

		// Added by us for filling out tables
		// A
		INTTYPE_REST n_paren_i_paren_circ_circ;
		INTTYPE_REST n_bracket_i_paren_circ_circ;
		CountingLinkedListNumOnly *n_j_arrow_i;
		INTTYPE_REST n_paren_i_paren_circ_square;
		INTTYPE_REST n_bracket_i_paren_circ_square;
		INTTYPE_REST n_i_arrow_circ_arrow_circ;
		INTTYPE_REST n_i_arrow_circ_arrow_square;
		INTTYPE_REST n_paren_circ_square_arrow_i;
		CountingLinkedListNumOnly *n_i_arrow_j;

		// New counters for calculating E
		INTTYPE_REST n_i_circ_square;
		INTTYPE_REST n_i_circ_arrow_square;
		INTTYPE_REST n_circ_square_arrow_i;
		INTTYPE_REST n_paren_i_circ_square;
		INTTYPE_REST n_0_i_circ;
		INTTYPE_REST n_i_circ_arrow_0;
		INTTYPE_REST n_0_i_arrow_circ;
		INTTYPE_REST n_0_circ_arrow_i;
		INTTYPE_REST n_paren_0_i_circ;
		INTTYPE_REST n_bracket_i_circ_square;
		INTTYPE_REST n_bracket_0_i_circ;
#endif

		// More general stuff
		unsigned int num;
		enum NodeType {Regular, End, Dummy};
		NodeType type;
		CountingLinkedList *next, *iterator;
		static CountingLinkedList dummyLL;
		bool n_i_j_is_reset, n_j_arrow_i_is_reset, n_i_arrow_j_is_reset;

		CountingLinkedList()
		{
		}

		void initialize()
		{
			next = NULL;
#ifdef quartetsToo
			n_i_j = NULL;
			n_j_arrow_i = n_i_arrow_j = NULL;
#endif
		}

		CountingLinkedList(bool dummy)
		{
			initialize();

			// Triplets
			n_i = 0;
			n_i_circ = 0;
			n_paren_ii = 0;
			n_i_arrow_circ = 0;

			// Quartets
#ifdef quartetsToo
			n_0_i = 0;
			n_ii = 0;
			n_0_paren_ii = 0;
			n_circ_paren_ii = 0;
			n_i_paren_0_circ = 0;
			n_i_paren_circ_circ = 0;
			n_i_paren_circ_square = 0;

			n_bracket_i_circ = 0;

			n_paren_0_i = 0;
			n_paren_i_circ = 0;
			n_paren_0_paren_ii = 0;
			n_paren_circ_paren_ii = 0;
			n_paren_i_paren_0_circ = 0;

			n_bracket_0_paren_ii = 0;
			n_bracket_circ_paren_ii = 0;
			n_bracket_i_paren_0_circ = 0;

			n_0_arrow_i = 0;
			n_i_arrow_0 = 0;
			n_i_arrow_i = 0;
			n_circ_arrow_i = 0;
			n_0_arrow_paren_ii = 0;
			n_i_arrow_paren_0_circ = 0;
			n_i_arrow_paren_circ_square = 0;
			n_circ_arrow_paren_ii = 0;
			n_i_arrow_0_circ = 0;
			n_i_arrow_circ_circ = 0;
			n_i_arrow_circ_square = 0;
			n_circ_arrow_ii = 0;
			n_paren_ii_arrow_0 = 0;
			n_paren_ii_arrow_circ = 0;
			n_paren_circ_circ_arrow_i = 0;
			n_0_arrow_i_arrow_i = 0;
			n_i_arrow_circ_arrow_0 = 0;
			n_i_arrow_0_arrow_circ = 0;
			n_circ_arrow_i_arrow_i = 0;

			n_i_arrow_paren_circ_circ = 0;
			n_0_arrow_ii = 0;

			n_0_arrow_i_circ = 0;

			// Added by us for filling out tables
			// A
			n_paren_i_paren_circ_circ = 0;
			n_bracket_i_paren_circ_circ = 0;
			n_paren_i_paren_circ_square = 0;
			n_bracket_i_paren_circ_square = 0;
			n_i_arrow_circ_arrow_circ = 0;
			n_i_arrow_circ_arrow_square = 0;
			n_paren_circ_square_arrow_i = 0;

			// New counters for calculating E
			n_i_circ_square = 0;
			n_i_circ_arrow_square = 0;
			n_circ_square_arrow_i = 0;
			n_paren_i_circ_square = 0;
			n_0_i_circ = 0;
			n_i_circ_arrow_0 = 0;
			n_0_i_arrow_circ = 0;
			n_0_circ_arrow_i = 0;
			n_paren_0_i_circ = 0;
			n_bracket_i_circ_square = 0;
			n_bracket_0_i_circ = 0;
#endif
		}

#ifdef quartetsToo
		void resetIterator()
		{
			iterator = this;
			if (n_i_j != NULL) n_i_j->resetIterator();
			n_i_j_is_reset = true;
			if (n_j_arrow_i != NULL) n_j_arrow_i->resetIterator();
			n_j_arrow_i_is_reset = true;
			if (n_i_arrow_j != NULL) n_i_arrow_j->resetIterator();
			n_i_arrow_j_is_reset = true;
		}

		unsigned int getIteratorNum()
		{
			return iterator->num;
		}

		bool iteratorHasEnded()
		{
			return iterator == NULL;
		}

		CountingLinkedList* getIteratorValue(unsigned int j)
		{
			while (!iteratorHasEnded() && iterator->num < j)
			{
				if (iterator->type == End) iterator = NULL;
				else iterator = iterator->next;
			}
			if (iteratorHasEnded() || iterator->num > j) return &dummyLL;
			/*iterator->num == j*/ return iterator;
		}

		CountingLinkedList* getCurrentIteratorValueAndIncrement()
		{
			if (iteratorHasEnded()) return NULL;
			CountingLinkedList *result = iterator;
			if (iterator->type == End) iterator = NULL;
			else iterator = iterator->next;
			return result;
		}
#endif
};

#endif