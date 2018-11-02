#ifndef HDT_FACTORY_H
#define HDT_FACTORY_H
#include "hdt.h"
class RootedTree; // forward declaration
class CountingLinkedList; // forward declaration
class CountingLinkedListNumOnly; // forward declaration

class HDTFactory
{
	private:
		HDT *createdHDTs;
		HDT *currentHDT;
		CountingLinkedList *createdLL;
		CountingLinkedList *currentLL;
		CountingLinkedListNumOnly *createdLLNO;
		CountingLinkedListNumOnly *currentLLNO;
		TemplatedLinkedList<HDT*> *createdTLL;
		TemplatedLinkedList<HDT*> *currentTLL;
		int hdtLocation, llLocation, llnoLocation, currentLocationTLL;
		int numD;
		MemoryAllocator<HDT> *memHDT;
		MemoryAllocator<CountingLinkedList> *memCLL;
		MemoryAllocator<CountingLinkedListNumOnly> *memCLLNO;
		MemoryAllocator<TemplatedLinkedList<HDT*> > *memTLL;

	public:
		HDTFactory(int numD, HDTFactory *copyMemAllocFrom = NULL);
		~HDTFactory();
		HDT* getHDT(HDT::NodeType type, RootedTree *link, bool doLink);
		CountingLinkedList* getLL();
		CountingLinkedListNumOnly* getLLNO();
		TemplatedLinkedList<HDT*>* getTemplatedLinkedList();
		void deleteTemplatedLinkedList();
		long long getSizeInRam();
};

#endif