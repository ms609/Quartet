#include "AbstractDistanceCalculator.h"

#include "hdt.h"
#include "hdt_factory.h"

void AbstractDistanceCalculator::count(RootedTree *v) {
  if (v->isLeaf() || v->n <= 2) {
    // This will make sure the entire subtree has color 0!
    v->colorSubtree(0);
    
#ifdef doExtractAndContract
    delete hdt->factory;
#endif
    
    return;
  }

  // v is not a leaf!
  // Find largest subtree
  TemplatedLinkedList<RootedTree*> *largest = v->children;
  int largestN = largest->data->n;
  TemplatedLinkedList<RootedTree*> *beforeLargest = NULL;
  TemplatedLinkedList<RootedTree*> *prev = v->children;
  for(TemplatedLinkedList<RootedTree*> *current = v->children->next; current != NULL; current = current->next) {
    if (current->data->n > largestN) {
      largest = current;
      beforeLargest = prev;
      largestN = largest->data->n;
    }
    prev = current;
  }
  if (beforeLargest != NULL) {
    beforeLargest->next = largest->next;
    largest->next = v->children;
    v->children = largest;
  }
  
  // Color i'th subtree (i > 1) with color i
  int c = 2;
  for(TemplatedLinkedList<RootedTree*> *current = v->children->next; current != NULL; current = current->next) {
    current->data->colorSubtree(c);
    c++;
  }

  // Update counters in the HDT
  hdt->updateCounters();
  updateCounters();
  
#ifdef doExtractAndContract
  // Extract
  RootedTree** extractedVersions = new RootedTree*[v->numChildren - 1];
  c = 0;
  for(TemplatedLinkedList<RootedTree*> *current = v->children->next; current != NULL; current = current->next) {
    if (current->data->isLeaf() || current->data->n <= 2) {
      extractedVersions[c] = NULL;
    } else {
      current->data->markHDTAlternative();
      RootedTree *extractedT2 = hdt->extractAndGoBack(t1->factory);
      extractedVersions[c] = extractedT2->contract();
      delete extractedT2->factory;
    }
    c++; // Weee :)
  }
#endif

  // Color to 0
  for(TemplatedLinkedList<RootedTree*> *current = v->children->next; current != NULL; current = current->next) {
    current->data->colorSubtree(0);
  }

  // Contract and recurse on 1st child
  RootedTree *firstChild = v->children->data;
  if (firstChild->isLeaf() || firstChild->n <= 2) {
    // Do "nothing" (except clean up and possibly color!)
#ifdef doExtractAndContract
    // Notice no recoloring here... It's not neccesary as it is extracted and contracted away,
    // and will actually cause an error if called with firstChild->colorSubtree(0) as t2 is linked
    // to a non-existing hdt (as we just deleted it) (we could wait with deleting it, but as we don't need the coloring why bother)
    delete hdt->factory;
#else
    firstChild->colorSubtree(0);
#endif
  } else {
#ifdef doExtractAndContract
    bool hdtTooBig = firstChild->n * CONTRACT_MAX_EXTRA_SIZE < hdt->leafCount();
    if (hdtTooBig) {
      HDT *newHDT;
      
      firstChild->markHDTAlternative();
      RootedTree *extractedT2 = hdt->extractAndGoBack(t1->factory);
      RootedTree *contractedT2 = extractedT2->contract();
      delete extractedT2->factory;
      newHDT = HDT::constructHDT(contractedT2, t1->maxDegree, dummyHDTFactory, true);
      delete contractedT2->factory;
      delete hdt->factory;
      hdt = newHDT;
    }
#endif
    count(firstChild);
    // HDT is deleted in recursive call!
  }
  
  // Color 1 and recurse
  c = 0;
  for(TemplatedLinkedList<RootedTree*> *current = v->children->next; current != NULL; current = current->next) {
    if (!current->data->isLeaf() && current->data->n > 2) {
#ifdef doExtractAndContract
      hdt = HDT::constructHDT(extractedVersions[c], t1->maxDegree, dummyHDTFactory, true);
      delete extractedVersions[c]->factory;
#endif
      
      current->data->colorSubtree(1);
      
      count(current->data);
    }
    c++; // Weee :)
    // HDT is deleted on recursive calls!
  }
  
#ifdef doExtractAndContract
  delete[] extractedVersions;
#endif
}

void AbstractDistanceCalculator::countChildren(RootedTree *t) {
  if (t->isLeaf()) {
    t->n = 1;
    return;
  }
  
  int nSum = 0;
  for(TemplatedLinkedList<RootedTree*> *i = t->children; i != NULL; i = i->next) {
    RootedTree *childI = i->data;
    countChildren(childI);
    nSum += childI->n;
  }
  t->n = nSum;
}
