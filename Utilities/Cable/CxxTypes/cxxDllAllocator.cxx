/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxDllAllocator.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/

#include "cxxDllAllocator.h"

#include <stdlib.h>

namespace _cxx_
{

/**
 * Allocate "length" bytes of memory, and return a pointer to it.
 */
void* DllAllocate(size_t length, DllAllocator<void>::const_pointer)
{
  return ((void*)(new char[length]));
}


/**
 * Free "length" bytes of memory.
 */
void DllDeallocate(void* buffer, size_t)
{
  delete [] ((char*)buffer);
}
  
} // namespace _cxx_
