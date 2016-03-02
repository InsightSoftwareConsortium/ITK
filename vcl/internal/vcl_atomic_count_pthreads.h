#ifndef vcl_atomic_count_pthreads_h_
#define vcl_atomic_count_pthreads_h_
//:
// \file
// \brief thread/SMP safe reference counter
// \author www.boost.org
// \verbatim
//  Modifications
//   Gehua Yang (DualAlign) - 28 Aug. 2008 - first port from Boost 1.36.0
// \endverbatim
//
//  boost/detail/atomic_count_pthreads.hpp
//
//  Copyright (c) 2001, 2002 Peter Dimov and Multi Media Ltd.
//
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)
//

#include <pthread.h>

//
//  The generic pthread_mutex-based implementation sometimes leads to
//    inefficiencies. Example: a class with two vcl_atomic_count members
//    can get away with a single mutex.
//
//  Users can detect this situation by checking BOOST_AC_USE_PTHREADS.
//

class vcl_atomic_count
{
 private:

    class scoped_lock
    {
    public:

        scoped_lock(pthread_mutex_t & m): m_(m)
        {
            pthread_mutex_lock(&m_);
        }

        ~scoped_lock()
        {
            pthread_mutex_unlock(&m_);
        }

    private:

        pthread_mutex_t & m_;
    };

 public:

    explicit vcl_atomic_count(long v): value_(v)
    {
        pthread_mutex_init(&mutex_, 0);
    }

    ~vcl_atomic_count()
    {
        pthread_mutex_destroy(&mutex_);
    }

    void operator++()
    {
        scoped_lock lock(mutex_);
        ++value_;
    }

    long operator--()
    {
        scoped_lock lock(mutex_);
        return --value_;
    }

    operator long() const
    {
        scoped_lock lock(mutex_);
        return value_;
    }

 private:

    vcl_atomic_count(vcl_atomic_count const &);
    vcl_atomic_count & operator=(vcl_atomic_count const &);

    mutable pthread_mutex_t mutex_;
    long value_;
};

#endif // #ifndef vcl_atomic_count_pthreads_h_
