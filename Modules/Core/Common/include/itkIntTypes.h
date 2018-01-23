/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkIntTypes_h
#define itkIntTypes_h

#include "itkMacro.h"

#include <cstdint>
#include <climits>

namespace itk
{
typedef::int8_t   int8_t;
typedef::uint8_t  uint8_t;
typedef::int16_t  int16_t;
typedef::uint16_t uint16_t;
typedef::int32_t  int32_t;
typedef::uint32_t uint32_t;
typedef::int64_t  int64_t;
typedef::uint64_t uint64_t;

typedef::int_least8_t   int_least8_t;
typedef::uint_least8_t  uint_least8_t;
typedef::int_least16_t  int_least16_t;
typedef::uint_least16_t uint_least16_t;
typedef::int_least32_t  int_least32_t;
typedef::uint_least32_t uint_least32_t;
typedef::int_least64_t  int_least64_t;
typedef::uint_least64_t uint_least64_t;

typedef::int_fast8_t   int_fast8_t;
typedef::uint_fast8_t  uint_fast8_t;
typedef::int_fast16_t  int_fast16_t;
typedef::uint_fast16_t uint_fast16_t;
typedef::int_fast32_t  int_fast32_t;
typedef::uint_fast32_t uint_fast32_t;
typedef::int_fast64_t  int_fast64_t;
typedef::uint_fast64_t uint_fast64_t;

typedef::intmax_t  intmax_t;
typedef::uintmax_t uintmax_t;

typedef::intptr_t  intptr_t;
typedef::uintptr_t uintptr_t;


#if defined(ITK_USE_64BITS_IDS) && ((ULLONG_MAX != ULONG_MAX) || (LLONG_MAX != LONG_MAX))

/** Any count of number of items (number of pixels in an image, number of
 *  points) (it is unsigned) */
typedef uint64_t      SizeValueType;

/** Same type as SizeValueType but when used as an Id (pointId, cellId,
 *  labelObjectId..)(it is unsigned) */
typedef SizeValueType IdentifierType;

/** The components of the Index array (they are signed) */
typedef int64_t       IndexValueType;

/** Differences between components of indexes, distance from one pointer
 *  to the origin of a buffer (it is signed) */
typedef int64_t       OffsetValueType;

#else

/** Any count of number of items (number of pixels in an image, number of
 *  points) (it is unsigned) */
typedef unsigned long     SizeValueType;

/** Same type as SizeValueType but when used as an Id (pointId, cellId,
 *  labelObjectId..)(it is unsigned) */
typedef SizeValueType IdentifierType;

/** The components of the Index array (they are signed) */
typedef signed long   IndexValueType;

/** Differences between components of indexes, distance from one pointer
 *  to the origin of a buffer (it is signed) */
typedef signed long   OffsetValueType;

#endif

/** Type to count and reference number of threads */
typedef unsigned int  ThreadIdType;

/** Type to count and reference the modification time of objects.
 * May in the future be replaced by SizeValueType to avoid overflows.
 */
typedef unsigned long ModifiedTimeType;

}

#endif  /* itkIntTypes_h */
