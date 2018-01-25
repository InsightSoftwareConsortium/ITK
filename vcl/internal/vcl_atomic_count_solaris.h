#ifndef vcl_atomic_count_solaris_h_
#define vcl_atomic_count_solaris_h_
//:
// \file
// \brief thread/SMP safe reference counter
// \author www.boost.org
// \verbatim
//  Modifications
//   Gehua Yang (DualAlign) - 28 Aug. 2008 - first port from Boost 1.36.0
// \endverbatim
//
//  boost/detail/atomic_count_solaris.hpp
//   based on: boost/detail/atomic_count_win32.hpp
//
//  Copyright (c) 2001-2005 Peter Dimov
//  Copyright (c) 2006 Michael van der Westhuizen
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <atomic.h>

class vcl_atomic_count
{
 public:

    explicit vcl_atomic_count( uint32_t v ): value_( v )
    {
    }

    long operator++()
    {
        return atomic_inc_32_nv( &value_ );
    }

    long operator--()
    {
        return atomic_dec_32_nv( &value_ );
    }

    operator uint32_t() const
    {
        return static_cast<uint32_t const volatile &>( value_ );
    }

 private:

    vcl_atomic_count( vcl_atomic_count const & );
    vcl_atomic_count & operator=( vcl_atomic_count const & );

    uint32_t value_;
};

#endif // #ifndef vcl_atomic_count_solaris_h_
