#ifndef vcl_emulation_tempbuf_h
#define vcl_emulation_tempbuf_h
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
 *
 * Copyright (c) 1996
 * Silicon Graphics Computer Systems, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Silicon Graphics makes no
 * representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied warranty.
 *
 * Copyright (c) 1997
 * Mark of the Unicorn, Inc.
 *
 * Permission to use, copy, modify, distribute and sell this software
 * and its documentation for any purpose is hereby granted without fee,
 * provided that the above copyright notice appear in all copies and
 * that both that copyright notice and this permission notice appear
 * in supporting documentation.  Mark of the Unicorn makes no
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

#include <vcl_cstddef.h>
//#include <vcl_climits.h>
//#include <vcl_cstdlib.h>
#include "vcl_pair.h"

template <class T>
vcl_pair<T*, vcl_ptrdiff_t> get_temporary_buffer(vcl_ptrdiff_t len, T*) {
  if (len > vcl_ptrdiff_t(INT_MAX / sizeof(T)))
    len = INT_MAX / sizeof(T);

  while (len > 0) {
    T* tmp = (T*) malloc((vcl_size_t)len * sizeof(T));
    if (tmp != 0)
      return vcl_pair<T*, vcl_ptrdiff_t>(tmp, len);
    len /= 2;
  }

  return vcl_pair<T*, vcl_ptrdiff_t>((T*)0, 0);
}

template <class T>
inline void return_temporary_buffer(T* p) {
  free(p);
}

// extension : an object describing (possibly partially filled
// with constructed objects) temporary buffer
// useful for convenient exception cleanup, also greatly reduces the
// parameters count of functions in vcl_algorithm.h

template <class T, VCL_DFL_TYPE_PARAM_STLDECL(Distance,vcl_ptrdiff_t)>
struct __stl_tempbuf
{
public:
    typedef T  value_type;
    typedef T* pointer;
    typedef Distance difference_type;

    __stl_tempbuf() : buf((T*)0,0), fill_pointer(0) {}
    __stl_tempbuf(Distance n) :
        buf(get_temporary_buffer(n, (T*)0)), fill_pointer(0)
    {}

    ~__stl_tempbuf()
    {
        if (capacity()!=0) {
            vcl_destroy(begin(), end());
            return_temporary_buffer(begin());
        }
    }

    pointer begin() { return buf.first; }
    pointer end()   { return buf.first+fill_pointer; }
    difference_type size()     const             { return fill_pointer; }
    bool   empty()    const                      { return size()==0; }
    difference_type max_size() const             { return buf.second; }
    difference_type capacity() const             { return buf.second; }
    // reflects change in initalized area
    void   adjust_size(difference_type len)      { fill_pointer=len; }
protected:
    vcl_pair<T*, vcl_ptrdiff_t> buf;
    difference_type fill_pointer;
};

#endif // vcl_emulation_tempbuf_h
