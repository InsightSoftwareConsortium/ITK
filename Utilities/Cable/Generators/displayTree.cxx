#include "sourceRep.h"
#include "configRep.h"

#include <cstdio>

/**
 * Display the declaration tree in a human-readable form.
 */
void DisplayTree(const source::Namespace* globalNamespace,
                 const configuration::Package*,
                 const char*)
{
  globalNamespace->Print(stdout, 0);
}
