#include <cstdio>

#include "internalRep.h"
#include "configRep.h"

/**
 * Display the declaration tree in a human-readable form.
 */
void DisplayTree(const source::Namespace* globalNamespace,
                 const configuration::Package*,
                 const char*)
{
  globalNamespace->Print(stdout, 0);
}
