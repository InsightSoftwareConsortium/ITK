/*=========================================================================
 *
 *  Copyright NumFOCUS
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
using int8_t = ::int8_t;
using uint8_t = ::uint8_t;
using int16_t = ::int16_t;
using uint16_t = ::uint16_t;
using int32_t = ::int32_t;
using uint32_t = ::uint32_t;
using int64_t = ::int64_t;
using uint64_t = ::uint64_t;

using int_least8_t = ::int_least8_t;
using uint_least8_t = ::uint_least8_t;
using int_least16_t = ::int_least16_t;
using uint_least16_t = ::uint_least16_t;
using int_least32_t = ::int_least32_t;
using uint_least32_t = ::uint_least32_t;
using int_least64_t = ::int_least64_t;
using uint_least64_t = ::uint_least64_t;

using int_fast8_t = ::int_fast8_t;
using uint_fast8_t = ::uint_fast8_t;
using int_fast16_t = ::int_fast16_t;
using uint_fast16_t = ::uint_fast16_t;
using int_fast32_t = ::int_fast32_t;
using uint_fast32_t = ::uint_fast32_t;
using int_fast64_t = ::int_fast64_t;
using uint_fast64_t = ::uint_fast64_t;

using intmax_t = ::intmax_t;
using uintmax_t = ::uintmax_t;

using intptr_t = ::intptr_t;
using uintptr_t = ::uintptr_t;


#if defined(ITK_USE_64BITS_IDS) && ((ULLONG_MAX != ULONG_MAX) || (LLONG_MAX != LONG_MAX))

/** Any count of number of items (number of pixels in an image, number of
 *  points) (it is unsigned) */
using SizeValueType = uint64_t;

/** Same type as SizeValueType but when used as an Id (pointId, cellId,
 *  labelObjectId..)(it is unsigned) */
using IdentifierType = SizeValueType;

/** The components of the Index array (they are signed) */
using IndexValueType = int64_t;

/** Differences between components of indexes, distance from one pointer
 *  to the origin of a buffer (it is signed) */
using OffsetValueType = int64_t;

#else

/** Any count of number of items (number of pixels in an image, number of
 *  points) (it is unsigned) */
using SizeValueType = unsigned long;

/** Same type as SizeValueType but when used as an Id (pointId, cellId,
 *  labelObjectId..)(it is unsigned) */
using IdentifierType = SizeValueType;

/** The components of the Index array (they are signed) */
using IndexValueType = signed long;

/** Differences between components of indexes, distance from one pointer
 *  to the origin of a buffer (it is signed) */
using OffsetValueType = signed long;

#endif

/** Type to count and reference number of threads */
using ThreadIdType = unsigned int;

/** Type to count and reference the modification time of objects */
using ModifiedTimeType = SizeValueType;

} // namespace itk

#endif /* itkIntTypes_h */
