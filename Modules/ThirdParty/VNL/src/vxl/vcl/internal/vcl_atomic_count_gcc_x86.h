#ifndef vcl_atomic_count_gcc_x86_h_
#define vcl_atomic_count_gcc_x86_h_
//:
// \file
// \brief thread/SMP safe reference counter
// \author www.boost.org
// \verbatim
//  Modifications
//   Gehua Yang (DualAlign) - 28 Aug. 2008 - first port from Boost 1.36.0
// \endverbatim
//
//  boost/detail/atomic_count_gcc_x86.hpp
//
//  vcl_atomic_count for g++ on 486+/AMD64
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

    explicit vcl_atomic_count( long v ) : value_( static_cast< int >( v ) ) {}

    void operator++()
    {
        __asm__
        (
            "lock\n\t"
            "incl %0":
            "+m"( value_ ): // output (%0)
            : // inputs
            "cc" // clobbers
        );
    }

    long operator--()
    {
        return atomic_exchange_and_add( &value_, -1 ) - 1;
    }

    operator long() const
    {
        return atomic_exchange_and_add( &value_, 0 );
    }

 private:

    vcl_atomic_count(vcl_atomic_count const &);
    vcl_atomic_count & operator=(vcl_atomic_count const &);

    mutable int value_;

 private:

    static int atomic_exchange_and_add( int * pw, int dv )
    {
        // int r = *pw;
        // *pw += dv;
        // return r;

        int r;

        __asm__ __volatile__
        (
            "lock\n\t"
            "xadd %1, %0":
            "+m"( *pw ), "=r"( r ): // outputs (%0, %1)
            "1"( dv ): // inputs (%2 == %1)
            "memory", "cc" // clobbers
        );

        return r;
    }
};

#endif // #ifndef vcl_atomic_count_gcc_x86_h_
