#ifndef vcl_atomic_count_sync_h_
#define vcl_atomic_count_sync_h_
//:
// \file
// \brief thread/SMP safe reference counter
// \author www.boost.org
// \verbatim
//  Modifications
//   Gehua Yang (DualAlign) - 28 Aug. 2008 - first port from Boost 1.36.0
// \endverbatim
//
//  boost/detail/atomic_count_sync.hpp
//
//  vcl_atomic_count for g++ 4.1+
//
//  http://gcc.gnu.org/onlinedocs/gcc-4.1.1/gcc/Atomic-Builtins.html
//
//  Copyright 2007 Peter Dimov
//
//  Distributed under the Boost Software License, Version 1.0. (See
//  accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
//

class vcl_atomic_count
{
 public:

    explicit vcl_atomic_count( long v ) : value_( v ) {}

    void operator++()
    {
        __sync_add_and_fetch( &value_, 1 );
    }

    long operator--()
    {
        return __sync_add_and_fetch( &value_, -1 );
    }

    operator long() const
    {
        return __sync_fetch_and_add( &value_, 0 );
    }

 private:

    vcl_atomic_count(vcl_atomic_count const &);
    vcl_atomic_count & operator=(vcl_atomic_count const &);

    mutable long value_;
};

#endif // #ifndef vcl_atomic_count_sync_h_
