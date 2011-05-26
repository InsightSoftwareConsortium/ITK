#ifndef vcl_atomic_count_win32_h_
#define vcl_atomic_count_win32_h_

// MS compatible compilers support #pragma once

#if defined(_MSC_VER) && (_MSC_VER >= 1020)
# pragma once
#endif
//:
// \file
// \brief thread/SMP safe reference counter
// \author www.boost.org
// \verbatim
//  Modifications
//   Gehua Yang (DualAlign) - 28 Aug. 2008 - first port from Boost 1.36.0
// \endverbatim
//
//  boost/detail/atomic_count_win32.hpp
//
//  Copyright (c) 2001-2005 Peter Dimov
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include "vcl_interlocked.h"

class vcl_atomic_count
{
 public:

    explicit vcl_atomic_count( long v ): value_( v )
    {
    }

    long operator++()
    {
        return BOOST_INTERLOCKED_INCREMENT( &value_ );
    }

    long operator--()
    {
        return BOOST_INTERLOCKED_DECREMENT( &value_ );
    }

    operator long() const
    {
        return static_cast<long const volatile &>( value_ );
    }

 private:

    vcl_atomic_count( vcl_atomic_count const & );
    vcl_atomic_count & operator=( vcl_atomic_count const & );

    long value_;
};

#endif // #ifndef vcl_atomic_count_win32_h_
