/*
 *
 * Copyright (c) 1994
 * Hewlett-Packard Company
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Hewlett-Packard Company makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 * Copyright (c) 1997
 * Moscow Center for SPARC Technology
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Moscow Center for SPARC Technology makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 */

#ifndef vcl_emulation_defalloc_h
#define vcl_emulation_defalloc_h

#include <vcl_new.h>
#include <vcl_cstddef.h>
//#include <vcl_cstdlib.h>
//#include <vcl_climits.h>
#include "vcl_alloc.h"
#include "vcl_algobase.h"

// This file is obsolete; provided only for backward compatibility
// with code that use vcl_allocator<T>

template <class T>
inline T* allocate(vcl_size_t size, T*) {
    return 0 == size ? 0 : ::operator new(size*sizeof(T));
}

template <class T>
inline void deallocate(T* buffer) {
    ::operator delete buffer;
}

template <class T>
inline void deallocate(T* buffer, vcl_size_t) {
    ::operator delete buffer;
}

template <class T>
class vcl_allocator : public vcl_alloc {
  typedef vcl_alloc super;
public:
  typedef T value_type;
  typedef T* pointer;
  typedef const T* const_pointer;
  typedef T& reference;
  typedef const T& const_reference;
  typedef vcl_size_t size_type;
  typedef vcl_ptrdiff_t difference_type;
  static T* allocate(vcl_size_t n) { return (T*)super::allocate(n * sizeof(T));}
//static T* allocate(void) { return super::allocate(sizeof(T)); }
  static void deallocate(T *p, vcl_size_t n) { super::deallocate(p, n * sizeof(T)); }
//static void deallocate(T *p) { super::deallocate(p); }
  static pointer address(reference x) { return (pointer)&x; }
  static const_pointer address(const_reference x) {
    return (const_pointer)&x;
  }
  static size_type max_size() {
    size_type sz((vcl_size_t)(-1)/sizeof(T));
    size_type msz(1);
    return vcl_max(msz, sz);
  }
  // CD2 requires that
  static T* allocate(vcl_size_t n, const void* ) { return (T*)super::allocate(n * sizeof(T));}
  void construct(pointer p, const value_type& val) { vcl_construct(p, val); }
  void destroy(pointer p) { vcl_destroy(p); }
};

template<class T1, class T2> inline
bool operator==(const vcl_allocator<T1>&, const vcl_allocator<T2>&) { return true; }
template<class T1, class T2> inline
bool operator!=(const vcl_allocator<T1>&, const vcl_allocator<T2>&) { return false; }

__STL_FULL_SPECIALIZATION class vcl_allocator<void> {
public:
  typedef void* pointer;
  typedef const void* const_pointer;
};

#endif // vcl_emulation_defalloc_h
