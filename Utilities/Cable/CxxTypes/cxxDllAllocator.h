/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxDllAllocator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _cxxDllAllocator_h
#define _cxxDllAllocator_h

#ifndef _cxxUtils_DllAllocator_include
do_not_include_cxxDllAllocator_directly__use_cxxUtils;
#endif

#include <memory>

namespace _cxx_
{

template <typename T>
class DllAllocator;

/**
 * Special allocator for "void".
 */
template <> class DllAllocator<void>: public std::allocator<void> {};

_cxx_EXPORT void* DllAllocate(size_t length, DllAllocator<void>::const_pointer);
_cxx_EXPORT void DllDeallocate(void* buffer, size_t);

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

  typedef typename StdAllocator::pointer pointer;

  /**
   * Default constructor just invokes the real allocator's default constructor.
   */
  DllAllocator(): StdAllocator() {}
  
  /**
   * Copy constructor just invokes the real allocator's copy constructor.
   */
  DllAllocator(const DllAllocator& a): StdAllocator(a) {}

#ifndef _cxx_STATIC_ALLOCATOR_METHODS
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
#else
  /**
   * Re-implement the allocate() method to be sure to allocate memory on only
   * one DLL's heap.
   */
  static pointer allocate(size_type n, DllAllocator<void>::const_pointer hint = 0)
    {
      return (pointer)DllAllocate(n*sizeof(T), hint);
    }

  /**
   * Re-implement the deallocate() method to free memory allocated by the
   * re-implementation of the allocate() mehtod.
   */
  static void deallocate(void* p, size_type n)
    {
      DllDeallocate(p, n);
    }
#endif
};

} // namespace _cxx_
  
#endif
