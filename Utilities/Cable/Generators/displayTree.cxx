#include <cstdio>

#include "internalRep.h"

/**
 * Display the declaration tree in a human-readable form.
 */
void displayTree(Namespace* globalNamespace)
{
  globalNamespace->Print(stdout, 0);
}
