#include <Rcpp.h>
#include "hdt.h"
#include "rooted_tree.h"
#include "hdt_factory.h"
#include "counting_linked_list.h"
#include "counting_linked_list_num_only.h"

bool HDT::gotoIteratorValueForList(CountingLinkedList *list, unsigned int num)
{
	if (list == NULL || list->iteratorHasEnded()) return false;
	list->getIteratorValue(num);
	return !list->iteratorHasEnded();
}

INTTYPE_REST HDT::getIteratorValueForNumList(CountingLinkedListNumOnly *list, unsigned int num)
{
	if (list == NULL) return 0;
	return list->getIteratorValue(num);
}

bool HDT::gotoIteratorValueForNumList(CountingLinkedListNumOnly *list, unsigned int num)
{
	if (list == NULL || list->iteratorHasEnded()) return false;
	list->getIteratorValue(num);
	return !list->iteratorHasEnded();
}

bool HDT::hasIteratorForNumListEnded(CountingLinkedListNumOnly *list)
{
	return list == NULL || list->iteratorHasEnded();
}

void HDT::addToNumList(CountingLinkedList *parent, AddToType list, unsigned int num, INTTYPE_REST value)
{
	if (value < 0)
	  Rcpp::stop("Unexpected error: Adding negative `value` to numList");
				
	if (value <= 0) return;

	CountingLinkedListNumOnly *theList;
	bool isReset = false;

	switch(list)
	{
		case i_j: {INITIALIZE_PAREN_AND_SET_LIST(n_i_j, n_i_j_is_reset); break;}
		case j_arrow_i: {INITIALIZE_PAREN_AND_SET_LIST(n_j_arrow_i, n_j_arrow_i_is_reset); break;}
		case i_arrow_j: {INITIALIZE_PAREN_AND_SET_LIST(n_i_arrow_j, n_i_arrow_j_is_reset); break;}
	  default: Rcpp::stop("HDT: Error adding to numlist");
	}

	if (!isReset)
	{
		// Go to the next one!
		if (theList->iterator->next == NULL) theList->iterator->next = factory->getLLNO();
		theList->iterator->type = CountingLinkedListNumOnly::Regular;
		theList = theList->iterator = theList->iterator->next;
	}

	theList->type = CountingLinkedListNumOnly::End;
	theList->num = num;
	theList->value = value;
}
