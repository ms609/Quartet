#include "rooted_tree_factory.h"
#include "rooted_tree.h"

RootedTreeFactory::RootedTreeFactory(RootedTreeFactory *copyMemAllocFrom)
{
	this->size = 30;

	if (copyMemAllocFrom == NULL)
	{
		memRT = new MemoryAllocator<RootedTree>(size+1);
		memTLL = new MemoryAllocator<TemplatedLinkedList<RootedTree*> >(size+1);
	}
	else
	{
		memRT = copyMemAllocFrom->memRT;
		memTLL = copyMemAllocFrom->memTLL;
	}
	memRT->numUses++;
	memTLL->numUses++;
	
	createdRT = memRT->getMemory();
	createdRT->altWorldSelf = NULL;
	currentRT = createdRT;
	currentLocationRT = 1;
	createdTLL = memTLL->getMemory();
	createdTLL->initialize();
	currentTLL = createdTLL;
	currentLocationTLL = 1;
}

RootedTreeFactory::~RootedTreeFactory()
{
	{
		RootedTree *current = createdRT;
		while (current != NULL)
		{
			RootedTree *next = current->altWorldSelf;
			memRT->releaseMemory(current);
			current = next;
		}
	}
	{
		TemplatedLinkedList<RootedTree*> *current = createdTLL;
		while (current != NULL)
		{
			TemplatedLinkedList<RootedTree*> *next = current->next;
			memTLL->releaseMemory(current);
			current = next;
		}
	}

	memRT->numUses--;
	if (memRT->numUses == 0) delete memRT;
	memTLL->numUses--;
	if (memTLL->numUses == 0) delete memTLL;
}

RootedTree* RootedTreeFactory::getRootedTree(string name)
{
	if (currentLocationRT > size)
	{
		currentRT->altWorldSelf = memRT->getMemory();
		currentRT = currentRT->altWorldSelf;
		currentRT->altWorldSelf = NULL;
		currentLocationRT = 1;
	}

	RootedTree *returnMe = &currentRT[currentLocationRT];
	returnMe->initialize(name);
	returnMe->factory = this;
	currentLocationRT++;
	return returnMe;
}

TemplatedLinkedList<RootedTree*>* RootedTreeFactory::getTemplatedLinkedList()
{
	if (currentLocationTLL > size)
	{
		currentTLL->next = memTLL->getMemory();
		currentTLL = currentTLL->next;
		currentTLL->initialize();
		currentLocationTLL = 1;
	}

	TemplatedLinkedList<RootedTree*> *returnMe = &currentTLL[currentLocationTLL];
	returnMe->initialize();
	currentLocationTLL++;
	return returnMe;
}

long long RootedTreeFactory::getSizeInRam()
{
	long long resultRT = 0;
	{
		RootedTree *current = createdRT;
		while (current != NULL)
		{
			resultRT++;
			current = current->altWorldSelf;
		}
	}

	long long resultTLL = 0;
	{
		TemplatedLinkedList<RootedTree*> *current = createdTLL;
		while (current != NULL)
		{
			resultTLL++;
			current = current->next;
		}
	}

	return resultRT * (size+1) * sizeof(RootedTree) + resultTLL * (size+1) * sizeof(TemplatedLinkedList<RootedTree*>);
}
