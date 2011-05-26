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

#ifndef vcl_emulation_pair_h
#define vcl_emulation_pair_h

#include "vcl_bool.h"

#undef struct
template <class T1, class T2>
class vcl_pair {
  public:
    typedef T1 first_type;
    typedef T2 second_type;

    T1 first;
    T2 second;
# if defined ( __STL_CONST_CONSTRUCTOR_BUG )
    vcl_pair() : first(T1()), second(T2()) {}
# else
    vcl_pair() {}
# endif
    vcl_pair(const T1& a, const T2& b) : first(a), second(b) {}
    // some compilers need that
    vcl_pair(const vcl_pair<T1,T2>& o) : first(o.first), second(o.second) {}
};

template <class T1, class T2>
inline bool operator==(const vcl_pair<T1, T2>& x, const vcl_pair<T1, T2>& y) {
    return x.first == y.first && x.second == y.second;
}

template <class T1, class T2>
inline bool operator<(const vcl_pair<T1, T2>& x, const vcl_pair<T1, T2>& y) {
    return (x.first < y.first) || (!(y.first < x.first) && x.second < y.second);
}

template <class T1, class T2>
inline vcl_pair<T1, T2> make_pair(const T1& x, const T2& y) {
    return vcl_pair<T1, T2>(x, y);
}

#endif // vcl_emulation_pair_h
