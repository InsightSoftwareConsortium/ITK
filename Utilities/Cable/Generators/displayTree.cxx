#include <cstdio>

#include "internalRep.h"
#include "configRep.h"

/**
 * Display the declaration tree in a human-readable form.
 */
void DisplayTree(const Namespace* globalNamespace,
                 const WrapperConfiguration*)
{
  globalNamespace->Print(stdout, 0);
}
