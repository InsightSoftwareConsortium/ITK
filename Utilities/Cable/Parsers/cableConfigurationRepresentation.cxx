#include "configRep.h"

/**
 * Construct a new Configuration and return a smart pointer to it.
 */
Configuration::Pointer
Configuration
::New(void)
{
  return new Configuration;
}

