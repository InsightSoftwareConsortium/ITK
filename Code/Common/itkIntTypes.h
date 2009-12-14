/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIntTypes.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkIntTypes_h
#define __itkIntTypes_h

#include "itkConfigure.h"

#if defined(ITK_HAVE_STDINT_H)
#include <stdint.h>
#else
// the system doesn't have the C or C++ version of stdint so lets use
// itksys's types for fixed widths
#include "itksys/FundamentalType.h"

#ifdef ITK_HAVE_STDDEF_H
#include <stddef.h>
#endif //ITK_HAVE_STDDEF_H

#if !defined(itksys_CAN_CONVERT_UI64_TO_DOUBLE) || ( itksys_SIZEOF_LONG != 8 || itksys_USE_LONG_LONG != 8 )
#define ITK_NO_INT_64
#endif

#endif // ITK_HAVE_CSTDINT


#if !defined(ITK_LEGACY_REMOVE)
#ifdef __cplusplus
extern "C" {
#endif

  /** Convenient and more descriptive integer types. */
  typedef char      ITK_INT8;
  typedef int       ITK_INT32;

#ifndef _WIN32
  typedef long long   ITK_INT64;
#endif

#ifdef _WIN32
  typedef long      ITK_INT64;
#endif

  typedef unsigned char   ITK_UINT8;
  typedef unsigned short  ITK_UINT16;
  typedef unsigned        ITK_UINT32;

#ifndef _WIN32
  typedef unsigned long long  ITK_UINT64;
#endif

#ifdef _WIN32
  typedef unsigned long ITK_UINT64;
#endif

  typedef int       ITK_INTPTR;
  typedef unsigned  ITK_UINTPTR;

#ifdef __cplusplus
}
#endif
#endif // ITK_LEGACY_REMOVE

namespace itk
{
#if defined(ITK_HAVE_STDINT_H) 

// Note: these types are technically optional in C99 stdint.h file. As
// such a try complile for their existance may be needed.
typedef ::int8_t   int8_t;
typedef ::uint8_t  uint8_t;
typedef ::int16_t  int16_t;
typedef ::uint16_t uint16_t;
typedef ::int32_t  int32_t;
typedef ::uint32_t uint32_t;
typedef ::int64_t  int64_t;
typedef ::uint64_t uint64_t;


// Note: these types are required for the C99 stdint.h file. However,
// not all C++ systems have a fully functional 64-bit integer.
typedef ::int_least8_t   int_least8_t;
typedef ::uint_least8_t  uint_least8_t;
typedef ::int_least16_t  int_least16_t;
typedef ::uint_least16_t uint_least16_t;
typedef ::int_least32_t  int_least32_t;
typedef ::uint_least32_t uint_least32_t;
typedef ::int_least64_t  int_least64_t;
typedef ::uint_least64_t uint_least64_t;


// Note: these types are required for the C99 stdint.h file. However,
// not all C++ systems have a fully functional 64-bit integer.
typedef ::int_fast8_t   int_fast8_t;
typedef ::uint_fast8_t  uint_fast8_t;
typedef ::int_fast16_t  int_fast16_t;
typedef ::uint_fast16_t uint_fast16_t;
typedef ::int_fast32_t  int_fast32_t;
typedef ::uint_fast32_t uint_fast32_t;
typedef ::int_fast64_t  int_fast64_t;
typedef ::uint_fast64_t uint_fast64_t;

typedef ::intmax_t  intmax_t;
typedef ::uintmax_t uintmax_t;

typedef ::intptr_t  intptr_t;
typedef ::uintptr_t uintptr_t;

#else // ITK_HAVE_STDINT_H || ITK_HAVE_CSTDINT

typedef ::itksysFundamentalType_Int8   int8_t;
typedef ::itksysFundamentalType_UInt8  uint8_t;
typedef ::itksysFundamentalType_Int16  int16_t;
typedef ::itksysFundamentalType_UInt16 uint16_t;
typedef ::itksysFundamentalType_Int32  int32_t;
typedef ::itksysFundamentalType_UInt32 uint32_t;

typedef int8_t   int_least8_t;
typedef uint8_t  uint_least8_t;
typedef int16_t  int_least16_t;
typedef uint16_t uint_least16_t;
typedef int32_t  int_least32_t;
typedef uint32_t uint_least32_t;

typedef int8_t   int_fast8_t;
typedef uint8_t  uint_fast8_t;
typedef int16_t  int_fast16_t;
typedef uint16_t uint_fast16_t;
typedef int32_t  int_fast32_t;
typedef uint32_t uint_fast32_t;

#ifdef ITK_NO_INT_64

typedef int32_t  intmax_t;
typedef uint32_t uintmax_t;

#else // ITK_NO_INT_64

typedef ::itksysFundamentalType_Int64  int64_t;
typedef ::itksysFundamentalType_UInt64 uint64_t;
typedef int64_t                        int_least64_t;
typedef uint64_t                       uint_least64_t;
typedef int64_t                        int_fast64_t;
typedef uint64_t                       uint_fast64_t;

typedef int64_t  intmax_t;
typedef uint64_t uintmax_t;

#endif // ITK_NO_INT_64

typedef ::ptrdiff_t intptr_t;
typedef ::size_t    uintptr_t;

#endif // ITK_HAVE_STDINT_H
}

#endif  /* __itkIntTypes_h */
