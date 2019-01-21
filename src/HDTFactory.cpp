#include "hdt_factory.h"
#include "rooted_tree.h"
#include "counting_linked_list.h"

CountingLinkedList CountingLinkedList::dummyLL = CountingLinkedList(true);

#define HDTFactorySize 30

HDTFactory::HDTFactory(int numD, HDTFactory *copyMemAllocFrom)
{
	this->numD = numD;

	if (copyMemAllocFrom == NULL)
	{
		memHDT = new MemoryAllocator<HDT>(HDTFactorySize+1);
		memCLL = new MemoryAllocator<CountingLinkedList>(HDTFactorySize+1);
		memCLLNO = new MemoryAllocator<CountingLinkedListNumOnly>(HDTFactorySize+1);
		memTLL = new MemoryAllocator<TemplatedLinkedList<HDT*> >(HDTFactorySize+1);
	}
	else
	{
		memHDT = copyMemAllocFrom->memHDT;
		memCLL = copyMemAllocFrom->memCLL;
		memCLLNO = copyMemAllocFrom->memCLLNO;
		memTLL = copyMemAllocFrom->memTLL;
	}
	memHDT->numUses++;
	memCLL->numUses++;
	memCLLNO->numUses++;
	memTLL->numUses++;

	createdHDTs = memHDT->getMemory();
	createdHDTs->left = NULL;
	currentHDT = createdHDTs;
	hdtLocation = 1;

	createdLL = memCLL->getMemory();
	createdLL->initialize();
	currentLL = createdLL;
	llLocation = 1;

	createdLLNO = memCLLNO->getMemory();
	createdLLNO->initialize();
	currentLLNO = createdLLNO;
	llnoLocation = 1;

	createdTLL = memTLL->getMemory();
	createdTLL->initialize();
	currentTLL = createdTLL;
	currentLocationTLL = 1;
}

HDTFactory::~HDTFactory()
{
	{
		HDT *current = createdHDTs;
		while (current != NULL)
		{
			HDT *next = current->left;
			memHDT->releaseMemory(current);
			current = next;
		}
	}

	{
		CountingLinkedList *current = createdLL;
		while (current != NULL)
		{
			CountingLinkedList *next = current->next;
			memCLL->releaseMemory(current);
			current = next;
		}
	}

	{
		CountingLinkedListNumOnly *current = createdLLNO;
		while (current != NULL)
		{
			CountingLinkedListNumOnly *next = current->next;
			memCLLNO->releaseMemory(current);
			current = next;
		}
	}

	{
		TemplatedLinkedList<HDT*> *current = createdTLL;
		while (current != NULL)
		{
			TemplatedLinkedList<HDT*> *next = current->next;
			memTLL->releaseMemory(current);
			current = next;
		}
	}
	
	memHDT->numUses--;
	if (memHDT->numUses == 0) delete memHDT;
	memCLL->numUses--;
	if (memCLL->numUses == 0) delete memCLL;
	memCLLNO->numUses--;
	if (memCLLNO->numUses == 0) delete memCLLNO;
	memTLL->numUses--;
	if (memTLL->numUses == 0) delete memTLL;
}

void HDTFactory::deleteTemplatedLinkedList()
{
	TemplatedLinkedList<HDT*> *current = createdTLL;
	while (current != NULL)
	{
		TemplatedLinkedList<HDT*> *next = current->next;
		memTLL->releaseMemory(current);
		current = next;
	}
	createdTLL = currentTLL = NULL;
}

HDT* HDTFactory::getHDT(HDT::NodeType type, RootedTree *link, bool doLink)
{
	if (hdtLocation > HDTFactorySize)
	{
		currentHDT->left = memHDT->getMemory();;
		currentHDT = currentHDT->left;
		currentHDT->left = NULL;
		hdtLocation = 1;
	}

	HDT *returnMe = &currentHDT[hdtLocation];
	returnMe->initialize(getLL(), type, numD, link, doLink);
	returnMe->factory = this;
	hdtLocation++;
	return returnMe;
}

CountingLinkedList* HDTFactory::getLL()
{
	if (llLocation > HDTFactorySize)
	{
		currentLL->next = memCLL->getMemory();;
		currentLL = currentLL->next;
		currentLL->initialize();
		llLocation = 1;
	}

	CountingLinkedList *returnMe = &currentLL[llLocation];
	returnMe->initialize();
	llLocation++;
	return returnMe;
}

CountingLinkedListNumOnly* HDTFactory::getLLNO()
{
	if (llnoLocation > HDTFactorySize)
	{
		currentLLNO->next = memCLLNO->getMemory();
		currentLLNO = currentLLNO->next;
		currentLLNO->initialize();
		llnoLocation = 1;
	}

	CountingLinkedListNumOnly *returnMe = &currentLLNO[llnoLocation];
	returnMe->initialize();
	llnoLocation++;
	return returnMe;
}

TemplatedLinkedList<HDT*>* HDTFactory::getTemplatedLinkedList()
{
	if (currentLocationTLL > HDTFactorySize)
	{
		currentTLL->next = memTLL->getMemory();
		currentTLL = currentTLL->next;
		currentTLL->initialize();
		currentLocationTLL = 1;
	}

	TemplatedLinkedList<HDT*> *returnMe = &currentTLL[currentLocationTLL];
	returnMe->initialize();
	currentLocationTLL++;
	return returnMe;
}

INTTYPE_N4 HDTFactory::getSizeInRam()
{
  INTTYPE_N4 resultHDT = 0;
	{
		HDT *current = createdHDTs;
		while (current != NULL)
		{
			resultHDT++;
			current = current->left;
		}
	}

	INTTYPE_N4 resultLL = 0;
	{
		CountingLinkedList *current = createdLL;
		while (current != NULL)
		{
			resultLL++;
			current = current->next;
		}
	}

	INTTYPE_N4 resultLLNO = 0;
	{
		CountingLinkedListNumOnly *current = createdLLNO;
		while (current != NULL)
		{
			resultLLNO++;
			current = current->next;
		}
	}

	INTTYPE_N4 resultTLL = 0;
	{
		TemplatedLinkedList<HDT*> *current = createdTLL;
		while (current != NULL)
		{
			resultTLL++;
			current = current->next;
		}
	}

	return resultHDT * (HDTFactorySize+1) * sizeof(HDT) +
		resultLL * (HDTFactorySize+1) * sizeof(CountingLinkedList) +
		resultLLNO * (HDTFactorySize+1) * sizeof(CountingLinkedListNumOnly) +
		resultTLL * (HDTFactorySize+1) * sizeof(TemplatedLinkedList<HDT*>);
}
