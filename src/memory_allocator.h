#ifndef MEMORY_ALLOCATOR_H
#define MEMORY_ALLOCATOR_H

struct voider
{
	voider *next;
};

template<class type>
class MemoryAllocator
{
	private:
		voider *freelist, *createdList, *currentList;
		unsigned int size;
		unsigned int chunks;
		
	public:
		unsigned int numUses;

		MemoryAllocator(unsigned int size)
		{
			this->size = size;
			freelist = createdList = currentList = NULL;
			numUses = 0;
			/*
				2mb (subtracting 2 types, 1 used for createdList and currentList,
				1 to make sure C++ have space for the "there's x of these" data)
				divided among "arrays" of size "size"
			*/
			chunks = (2 * 1024 * 1024 - 2 * sizeof(type)) / (sizeof(type) * size);
			getMoreSpace();
		}
		
		~MemoryAllocator()
		{
			voider *current = createdList;
			while (current != NULL)
			{
				voider *next = current->next;
				type *asRealType = reinterpret_cast<type*>(current);
				delete[] asRealType;
				current = next;
			}
		}

		void getMoreSpace()
		{
			type *asRealType = new type[size * chunks + 1];
			if (createdList == NULL)
			{
				createdList = reinterpret_cast<voider*>(asRealType);
				createdList->next = NULL;
				currentList = createdList;
			}
			else
			{
				currentList->next = reinterpret_cast<voider*>(asRealType);
				currentList = currentList->next;
				currentList->next = NULL;
			}

			for(unsigned int i = 0; i < chunks; i++)
			{
				releaseMemory(&asRealType[i * size + 1]);
			}
		}
		
		type* getMemory()
		{
			if (freelist == NULL)
			{
				getMoreSpace();
			}

			voider *returnThis = freelist;
			freelist = freelist->next;
			return reinterpret_cast<type*>(returnThis); // cast (and don't call constructor)
		}
		
		void releaseMemory(type *mem)
		{
			voider *casted = reinterpret_cast<voider*>(mem);
			casted->next = freelist;
			freelist = casted;
		}
};

#endif