#ifndef vcl_atomic_count_gcc_h_
#define vcl_atomic_count_gcc_h_
//:
// \file
// \brief thread/SMP safe reference counter
// \author www.boost.org
// \verbatim
//  Modifications
//   Gehua Yang (DualAlign) - 28 Aug. 2008 - first port from Boost 1.36.0
// \endverbatim
//
//  boost/detail/atomic_count_gcc.hpp
//
//  vcl_atomic_count for GNU libstdc++ v3
//
//  http://gcc.gnu.org/onlinedocs/porting/Thread-safety.html
//
//  Copyright (c) 2001, 2002 Peter Dimov and Multi Media Ltd.
//  Copyright (c) 2002 Lars Gullik Bjønnes <larsbj@lyx.org>
//  Copyright 2003-2005 Peter Dimov
//
//  Distributed under the Boost Software License, Version 1.0. (See
//  accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//

#include <bits/atomicity.h>


#if defined(__GLIBCXX__) // g++ 3.4+

using __gnu_cxx::__atomic_add;
using __gnu_cxx::__exchange_and_add;

#endif

class vcl_atomic_count
{
 public:

    explicit vcl_atomic_count(long v) : value_(v) {}

    void operator++()
    {
        __atomic_add(&value_, 1);
    }

    long operator--()
    {
        return __exchange_and_add(&value_, -1) - 1;
    }

    operator long() const
    {
        return __exchange_and_add(&value_, 0);
    }

 private:

    vcl_atomic_count(vcl_atomic_count const &);
    vcl_atomic_count & operator=(vcl_atomic_count const &);

    mutable _Atomic_word value_;
};

#endif // #ifndef vcl_atomic_count_gcc_h_
