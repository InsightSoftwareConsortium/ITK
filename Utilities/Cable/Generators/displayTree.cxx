#include <cstdio>

#include "internalRep.h"
#include "configRep.h"

/**
 * Display the declaration tree in a human-readable form.
 */
void displayTree(Namespace* globalNamespace,
                 WrapperConfiguration*)
{
  globalNamespace->Print(stdout, 0);
}
