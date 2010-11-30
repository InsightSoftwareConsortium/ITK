#ifndef vcl_interlocked_h_
#define vcl_interlocked_h_

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
//  boost/detail/interlocked.hpp
//
//  Copyright 2005 Peter Dimov
//
//  Distributed under the Boost Software License, Version 1.0. (See
//  accompanying file LICENSE_1_0.txt or copy at
//  http://www.boost.org/LICENSE_1_0.txt)
 //

// for the problematic Visual Studio 6
#if defined(_WIN32) || defined(WIN32)
#  if _MSC_VER < 1300
#    define VCL_INTERLOCKED_VC6
#  endif
#endif

//
// Do we need this?
#if defined( __BORLANDC__ ) || defined( VCL_INTERLOCKED_VC6 )

# define NOMINMAX
# include <windows.h>

# define BOOST_INTERLOCKED_INCREMENT InterlockedIncrement
# define BOOST_INTERLOCKED_DECREMENT InterlockedDecrement
# define BOOST_INTERLOCKED_COMPARE_EXCHANGE InterlockedCompareExchange
# define BOOST_INTERLOCKED_EXCHANGE InterlockedExchange
# define BOOST_INTERLOCKED_EXCHANGE_ADD InterlockedExchangeAdd
# define BOOST_INTERLOCKED_COMPARE_EXCHANGE_POINTER InterlockedCompareExchangePointer
# define BOOST_INTERLOCKED_EXCHANGE_POINTER InterlockedExchangePointer

#elif defined(_WIN32_WCE)

// under Windows CE we still have old-style Interlocked* functions

  extern "C" long __cdecl InterlockedIncrement( long* );
  extern "C" long __cdecl InterlockedDecrement( long* );
  extern "C" long __cdecl InterlockedCompareExchange( long*, long, long );
  extern "C" long __cdecl InterlockedExchange( long*, long );
  extern "C" long __cdecl InterlockedExchangeAdd( long*, long );

# define BOOST_INTERLOCKED_INCREMENT InterlockedIncrement
# define BOOST_INTERLOCKED_DECREMENT InterlockedDecrement
# define BOOST_INTERLOCKED_COMPARE_EXCHANGE InterlockedCompareExchange
# define BOOST_INTERLOCKED_EXCHANGE InterlockedExchange
# define BOOST_INTERLOCKED_EXCHANGE_ADD InterlockedExchangeAdd

# define BOOST_INTERLOCKED_COMPARE_EXCHANGE_POINTER(dest,exchange,compare) \
    ((void*)BOOST_INTERLOCKED_COMPARE_EXCHANGE((long*)(dest),(long)(exchange),(long)(compare)))
# define BOOST_INTERLOCKED_EXCHANGE_POINTER(dest,exchange) \
    ((void*)BOOST_INTERLOCKED_EXCHANGE((long*)(dest),(long)(exchange)))

#elif defined( _MSC_VER ) || defined( __ICC )

  extern "C" long __cdecl _InterlockedIncrement( long volatile * );
  extern "C" long __cdecl _InterlockedDecrement( long volatile * );
  extern "C" long __cdecl _InterlockedCompareExchange( long volatile *, long, long );
  extern "C" long __cdecl _InterlockedExchange( long volatile *, long);
  extern "C" long __cdecl _InterlockedExchangeAdd( long volatile *, long);

# pragma intrinsic( _InterlockedIncrement )
# pragma intrinsic( _InterlockedDecrement )
# pragma intrinsic( _InterlockedCompareExchange )
# pragma intrinsic( _InterlockedExchange )
# pragma intrinsic( _InterlockedExchangeAdd )

# if defined(_M_IA64) || defined(_M_AMD64)

    extern "C" void* __cdecl _InterlockedCompareExchangePointer( void* volatile *, void*, void* );
    extern "C" void* __cdecl _InterlockedExchangePointer( void* volatile *, void* );

#   pragma intrinsic( _InterlockedCompareExchangePointer )
#   pragma intrinsic( _InterlockedExchangePointer )

#   define BOOST_INTERLOCKED_COMPARE_EXCHANGE_POINTER _InterlockedCompareExchangePointer
#   define BOOST_INTERLOCKED_EXCHANGE_POINTER _InterlockedExchangePointer

# else

#   define BOOST_INTERLOCKED_COMPARE_EXCHANGE_POINTER(dest,exchange,compare) \
      ((void*)BOOST_INTERLOCKED_COMPARE_EXCHANGE((long volatile*)(dest),(long)(exchange),(long)(compare)))
#   define BOOST_INTERLOCKED_EXCHANGE_POINTER(dest,exchange) \
      ((void*)BOOST_INTERLOCKED_EXCHANGE((long volatile*)(dest),(long)(exchange)))

# endif

# define BOOST_INTERLOCKED_INCREMENT _InterlockedIncrement
# define BOOST_INTERLOCKED_DECREMENT _InterlockedDecrement
# define BOOST_INTERLOCKED_COMPARE_EXCHANGE _InterlockedCompareExchange
# define BOOST_INTERLOCKED_EXCHANGE _InterlockedExchange
# define BOOST_INTERLOCKED_EXCHANGE_ADD _InterlockedExchangeAdd

#elif defined( WIN32 ) || defined( _WIN32 ) || defined( __WIN32__ ) || defined( __CYGWIN__ )

  extern "C" __declspec(dllimport) long __stdcall InterlockedIncrement( long volatile * );
  extern "C" __declspec(dllimport) long __stdcall InterlockedDecrement( long volatile * );
  extern "C" __declspec(dllimport) long __stdcall InterlockedCompareExchange( long volatile *, long, long );
  extern "C" __declspec(dllimport) long __stdcall InterlockedExchange( long volatile *, long );
  extern "C" __declspec(dllimport) long __stdcall InterlockedExchangeAdd( long volatile *, long );

# define BOOST_INTERLOCKED_INCREMENT InterlockedIncrement
# define BOOST_INTERLOCKED_DECREMENT InterlockedDecrement
# define BOOST_INTERLOCKED_COMPARE_EXCHANGE InterlockedCompareExchange
# define BOOST_INTERLOCKED_EXCHANGE InterlockedExchange
# define BOOST_INTERLOCKED_EXCHANGE_ADD InterlockedExchangeAdd

# define BOOST_INTERLOCKED_COMPARE_EXCHANGE_POINTER(dest,exchange,compare) \
    ((void*)BOOST_INTERLOCKED_COMPARE_EXCHANGE((long volatile*)(dest),(long)(exchange),(long)(compare)))
# define BOOST_INTERLOCKED_EXCHANGE_POINTER(dest,exchange) \
    ((void*)BOOST_INTERLOCKED_EXCHANGE((long volatile*)(dest),(long)(exchange)))

#else

# error "Interlocked intrinsics not available"

#endif

#endif // #ifndef vcl_interlocked_h_
