/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxDllAllocator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _wrapDllAllocator_h
#define _wrapDllAllocator_h

#include <memory>

template <typename T>
class DllAllocator;

/**
 * Special allocator for "void".
 */
template <> class DllAllocator<void>: public std::allocator<void> {};

__declspec ( dllexport ) void* DllAllocate(size_t length, DllAllocator<void>::const_pointer);
__declspec ( dllexport ) void DllDeallocate(void* buffer, size_t);

/**
 * Define a replacement for the std::allocator class that is DLL-boundary
 * safe.  This is identical to the original class, except that it
 * re-implements the allocate and deallocate methods.  The new
 * implementations call non-templated allocate/deallocate functions that
 * exist in only one DLL.  This way the memory is always allocated and freed
 * on the heap for one DLL, and no boundary errors are created.
 */
template <typename T>
class DllAllocator: public std::allocator<T>
{
public:
  /**
   * Easy access to the allocator subclass from which we are taking most
   * of the implementation.
   */
  typedef std::allocator<T>  StdAllocator;

  /**
   * Default constructor just invokes the real allocator's default constructor.
   */
  DllAllocator(): StdAllocator() {}
  
  /**
   * Copy constructor just invokes the real allocator's copy constructor.
   */
  DllAllocator(const DllAllocator& a): StdAllocator(a) {}

  /**
   * Re-implement the allocate() method to be sure to allocate memory on only
   * one DLL's heap.
   */
  pointer allocate(size_type n, DllAllocator<void>::const_pointer hint = 0)
    {
      return (pointer)DllAllocate(n*sizeof(T), hint);
    }
  
  /**
   * Re-implement the deallocate() method to free memory allocated by the
   * re-implementation of the allocate() mehtod.
   */
  void deallocate(pointer p, size_type n)
    {
      DllDeallocate(p, n);
    }
};

#endif
