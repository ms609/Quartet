#ifndef COUNTING_LINKED_LIST_NUM_ONLY_H
#define COUNTING_LINKED_LIST_NUM_ONLY_H

#include "int_stuff.h"

class CountingLinkedListNumOnly
{
	public:
		INTTYPE_REST value;
		unsigned int num;
		enum NodeType {Regular, End, SkipAndEnd};
		NodeType type;
		CountingLinkedListNumOnly *next, *iterator;

		void initialize()
		{
			next = NULL;
		}

		void resetIterator()
		{
			iterator = this;
		}

		unsigned int getIteratorNum()
		{
			return iterator->num;
		}

		bool iteratorHasEnded()
		{
			return iterator == NULL || iterator->type == SkipAndEnd;
		}

		INTTYPE_REST getIteratorValue(unsigned int j)
		{
			while (!iteratorHasEnded() && iterator->num < j)
			{
				if (iterator->type == End) iterator = NULL;
				else iterator = iterator->next;
			}
			if (iteratorHasEnded() || iterator->num > j) return 0;
			/*iterator->num == j*/ return iterator->value;
		}
};

#endif
