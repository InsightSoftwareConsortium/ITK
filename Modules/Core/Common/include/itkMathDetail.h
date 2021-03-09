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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#ifndef itkMathDetail_h
#define itkMathDetail_h

#include "itkIntTypes.h"
#include "itkNumericTraits.h"

#include <cfenv>

#if (defined(ITK_COMPILER_SUPPORTS_SSE2_32) || defined(ITK_COMPILER_SUPPORTS_SSE2_64)) && !defined(ITK_WRAPPING_PARSER)
#  include <emmintrin.h> // SSE2 intrinsics
#endif

// Initially assume no SSE2:
#define USE_SSE2_64IMPL 0
#define USE_SSE2_32IMPL 0

// Turn on 32-bit and/or 64-bit SSE2 impl when using any compiler on x86 platform.
#if defined(ITK_COMPILER_SUPPORTS_SSE2_32) && !defined(ITK_WRAPPING_PARSER)
#  undef USE_SSE2_32IMPL
#  define USE_SSE2_32IMPL 1
#endif
#if defined(ITK_COMPILER_SUPPORTS_SSE2_64) && !defined(ITK_WRAPPING_PARSER)
#  undef USE_SSE2_64IMPL
#  define USE_SSE2_64IMPL 1
#endif

// Turn on 32-bit and 64-bit asm impl when using GCC/clang on x86 platform with the
// following exception:
//   GCCXML
#if defined(__GNUC__) && !defined(ITK_WRAPPING_PARSER) &&                                                              \
  (defined(__i386__) || defined(__i386) || defined(__x86_64__) || defined(__x86_64))
#  define GCC_USE_ASM_32IMPL 1
#  define GCC_USE_ASM_64IMPL 1
#else
#  define GCC_USE_ASM_32IMPL 0
#  define GCC_USE_ASM_64IMPL 0
#endif

// Turn on 32-bit and 64-bit asm impl when using MSVC on 32-bit x86 Windows
#if defined(_MSC_VER) && !defined(ITK_WRAPPING_PARSER) && !defined(_WIN64)
#  define VC_USE_ASM_32IMPL 1
#  define VC_USE_ASM_64IMPL 1
#else
#  define VC_USE_ASM_32IMPL 0
#  define VC_USE_ASM_64IMPL 0
#endif

namespace itk
{
namespace Math
{
namespace Detail
{
// The functions defined in this namespace are not meant to be used directly
// and thus do not adhere to the standard backward-compatibility
// policy of ITK, as any Detail namespace should be considered private.
// Please use the functions from the itk::Math namespace instead

////////////////////////////////////////
// Base versions

CLANG_PRAGMA_PUSH
CLANG_SUPPRESS_Wfloat_equal

  template <typename TReturn, typename TInput>
  inline TReturn
  RoundHalfIntegerToEven_base(TInput x)
{
  if (NumericTraits<TInput>::IsNonnegative(x))
  {
    x += static_cast<TInput>(0.5);
  }
  else
  {
    x -= static_cast<TInput>(0.5);
  }

  const auto r = static_cast<TReturn>(x);
  return (x != static_cast<TInput>(r)) ? r : static_cast<TReturn>(2 * (r / 2));
}

template <typename TReturn, typename TInput>
inline TReturn
RoundHalfIntegerUp_base(TInput x)
{
  x += static_cast<TInput>(0.5);
  const auto r = static_cast<TReturn>(x);
  return (NumericTraits<TInput>::IsNonnegative(x)) ? r
                                                   : (x == static_cast<TInput>(r) ? r : r - static_cast<TReturn>(1));
}

template <typename TReturn, typename TInput>
inline TReturn
Floor_base(TInput x)
{
  const auto r = static_cast<TReturn>(x);

  return (NumericTraits<TInput>::IsNonnegative(x)) ? r
                                                   : (x == static_cast<TInput>(r) ? r : r - static_cast<TReturn>(1));
}

template <typename TReturn, typename TInput>
inline TReturn
Ceil_base(TInput x)
{
  const auto r = static_cast<TReturn>(x);

  return (NumericTraits<TInput>::IsNegative(x)) ? r : (x == static_cast<TInput>(r) ? r : r + static_cast<TReturn>(1));
}

CLANG_PRAGMA_POP

////////////////////////////////////////
// 32 bits versions

#if USE_SSE2_32IMPL // SSE2 implementation

inline int32_t
RoundHalfIntegerToEven_32(double x)
{
#  if defined(ITK_CHECK_FPU_ROUNDING_MODE)
  itkAssertInDebugAndIgnoreInReleaseMacro(fegetround() == FE_TONEAREST);
#  endif
  return _mm_cvtsd_si32(_mm_set_sd(x));
}

inline int32_t
RoundHalfIntegerToEven_32(float x)
{
#  if defined(ITK_CHECK_FPU_ROUNDING_MODE)
  itkAssertInDebugAndIgnoreInReleaseMacro(fegetround() == FE_TONEAREST);
#  endif
  return _mm_cvtss_si32(_mm_set_ss(x));
}

#elif GCC_USE_ASM_32IMPL // GCC/clang x86 asm implementation

inline int32_t
RoundHalfIntegerToEven_32(double x)
{
#  if defined(ITK_CHECK_FPU_ROUNDING_MODE)
  itkAssertInDebugAndIgnoreInReleaseMacro(fegetround() == FE_TONEAREST);
#  endif
  int32_t r;
  __asm__ __volatile__("fistpl %0" : "=m"(r) : "t"(x) : "st");
  return r;
}

inline int32_t
RoundHalfIntegerToEven_32(float x)
{
#  if defined(ITK_CHECK_FPU_ROUNDING_MODE)
  itkAssertInDebugAndIgnoreInReleaseMacro(fegetround() == FE_TONEAREST);
#  endif
  int32_t r;
  __asm__ __volatile__("fistpl %0" : "=m"(r) : "t"(x) : "st");
  return r;
}

#elif VC_USE_ASM_32IMPL // msvc asm implementation

inline int32_t
RoundHalfIntegerToEven_32(double x)
{
#  if defined(ITK_CHECK_FPU_ROUNDING_MODE)
  itkAssertInDebugAndIgnoreInReleaseMacro(fegetround() == FE_TONEAREST);
#  endif
  int32_t r;
  __asm
  {
    fld x
    fistp r
  }
  return r;
}

inline int32_t
RoundHalfIntegerToEven_32(float x)
{
#  if defined(ITK_CHECK_FPU_ROUNDING_MODE)
  itkAssertInDebugAndIgnoreInReleaseMacro(fegetround() == FE_TONEAREST);
#  endif
  int32_t r;
  __asm
  {
    fld x
    fistp r
  }
  return r;
}

#else // Base implementation

inline int32_t
RoundHalfIntegerToEven_32(double x)
{
  return RoundHalfIntegerToEven_base<int32_t, double>(x);
}
inline int32_t
RoundHalfIntegerToEven_32(float x)
{
  return RoundHalfIntegerToEven_base<int32_t, float>(x);
}

#endif

#if USE_SSE2_32IMPL || GCC_USE_ASM_32IMPL || VC_USE_ASM_32IMPL

inline int32_t
RoundHalfIntegerUp_32(double x)
{
  return RoundHalfIntegerToEven_32(2 * x + 0.5) >> 1;
}
inline int32_t
RoundHalfIntegerUp_32(float x)
{
  return RoundHalfIntegerToEven_32(2 * x + 0.5f) >> 1;
}

inline int32_t
Floor_32(double x)
{
  return RoundHalfIntegerToEven_32(2 * x - 0.5) >> 1;
}
inline int32_t
Floor_32(float x)
{
  return RoundHalfIntegerToEven_32(2 * x - 0.5f) >> 1;
}

inline int32_t
Ceil_32(double x)
{
  return -(RoundHalfIntegerToEven_32(-0.5 - 2 * x) >> 1);
}
inline int32_t
Ceil_32(float x)
{
  return -(RoundHalfIntegerToEven_32(-0.5f - 2 * x) >> 1);
}

#else // Base implementation

inline int32_t
RoundHalfIntegerUp_32(double x)
{
  return RoundHalfIntegerUp_base<int32_t, double>(x);
}
inline int32_t
RoundHalfIntegerUp_32(float x)
{
  return RoundHalfIntegerUp_base<int32_t, float>(x);
}

inline int32_t
Floor_32(double x)
{
  return Floor_base<int32_t, double>(x);
}
inline int32_t
Floor_32(float x)
{
  return Floor_base<int32_t, float>(x);
}

inline int32_t
Ceil_32(double x)
{
  return Ceil_base<int32_t, double>(x);
}
inline int32_t
Ceil_32(float x)
{
  return Ceil_base<int32_t, float>(x);
}

#endif // USE_SSE2_32IMPL || GCC_USE_ASM_32IMPL || VC_USE_ASM_32IMPL

////////////////////////////////////////
// 64 bits versions

#if USE_SSE2_64IMPL // SSE2 implementation

inline int64_t
RoundHalfIntegerToEven_64(double x)
{
#  if defined(ITK_CHECK_FPU_ROUNDING_MODE)
  itkAssertInDebugAndIgnoreInReleaseMacro(fegetround() == FE_TONEAREST);
#  endif
  return _mm_cvtsd_si64(_mm_set_sd(x));
}

inline int64_t
RoundHalfIntegerToEven_64(float x)
{
#  if defined(ITK_CHECK_FPU_ROUNDING_MODE)
  itkAssertInDebugAndIgnoreInReleaseMacro(fegetround() == FE_TONEAREST);
#  endif
  return _mm_cvtss_si64(_mm_set_ss(x));
}

#elif GCC_USE_ASM_64IMPL // GCC/clang x86 asm implementation

inline int64_t
RoundHalfIntegerToEven_64(double x)
{
#  if defined(ITK_CHECK_FPU_ROUNDING_MODE)
  itkAssertInDebugAndIgnoreInReleaseMacro(fegetround() == FE_TONEAREST);
#  endif
  int64_t r;
  __asm__ __volatile__("fistpll %0" : "=m"(r) : "t"(x) : "st");
  return r;
}

inline int64_t
RoundHalfIntegerToEven_64(float x)
{
#  if defined(ITK_CHECK_FPU_ROUNDING_MODE)
  itkAssertInDebugAndIgnoreInReleaseMacro(fegetround() == FE_TONEAREST);
#  endif
  int64_t r;
  __asm__ __volatile__("fistpll %0" : "=m"(r) : "t"(x) : "st");
  return r;
}

#elif VC_USE_ASM_64IMPL // msvc asm implementation

inline int64_t
RoundHalfIntegerToEven_64(double x)
{
#  if defined(ITK_CHECK_FPU_ROUNDING_MODE)
  itkAssertInDebugAndIgnoreInReleaseMacro(fegetround() == FE_TONEAREST);
#  endif
  int64_t r;
  __asm
  {
    fld x
    fistp r
  }
  return r;
}

inline int64_t
RoundHalfIntegerToEven_64(float x)
{
#  if defined(ITK_CHECK_FPU_ROUNDING_MODE)
  itkAssertInDebugAndIgnoreInReleaseMacro(fegetround() == FE_TONEAREST);
#  endif
  int64_t r;
  __asm
  {
    fld x
    fistp r
  }
  return r;
}

#else // Base implementation

inline int64_t
RoundHalfIntegerToEven_64(double x)
{
  return RoundHalfIntegerToEven_base<int64_t, double>(x);
}
inline int64_t
RoundHalfIntegerToEven_64(float x)
{
  return RoundHalfIntegerToEven_base<int64_t, float>(x);
}

#endif

#if USE_SSE2_64IMPL || GCC_USE_ASM_64IMPL || VC_USE_ASM_64IMPL

inline int64_t
RoundHalfIntegerUp_64(double x)
{
  return RoundHalfIntegerToEven_64(2 * x + 0.5) >> 1;
}
inline int64_t
RoundHalfIntegerUp_64(float x)
{
  return RoundHalfIntegerToEven_64(2 * x + 0.5f) >> 1;
}

inline int64_t
Floor_64(double x)
{
  return RoundHalfIntegerToEven_64(2 * x - 0.5) >> 1;
}
inline int64_t
Floor_64(float x)
{
  return RoundHalfIntegerToEven_64(2 * x - 0.5f) >> 1;
}

inline int64_t
Ceil_64(double x)
{
  return -(RoundHalfIntegerToEven_64(-0.5 - 2 * x) >> 1);
}
inline int64_t
Ceil_64(float x)
{
  return -(RoundHalfIntegerToEven_64(-0.5f - 2 * x) >> 1);
}

#else // Base implementation

inline int64_t
RoundHalfIntegerUp_64(double x)
{
  return RoundHalfIntegerUp_base<int64_t, double>(x);
}
inline int64_t
RoundHalfIntegerUp_64(float x)
{
  return RoundHalfIntegerUp_base<int64_t, float>(x);
}

inline int64_t
Floor_64(double x)
{
  return Floor_base<int64_t, double>(x);
}
inline int64_t
Floor_64(float x)
{
  return Floor_base<int64_t, float>(x);
}

inline int64_t
Ceil_64(double x)
{
  return Ceil_base<int64_t, double>(x);
}
inline int64_t
Ceil_64(float x)
{
  return Ceil_base<int64_t, float>(x);
}

#endif // USE_SSE2_64IMPL || GCC_USE_ASM_64IMPL || VC_USE_ASM_64IMPL

template <typename T>
struct FloatIEEETraits;

template <>
struct FloatIEEETraits<float>
{
  using IntType = int32_t;
  using UIntType = uint32_t;
};

template <>
struct FloatIEEETraits<double>
{
  using IntType = int64_t;
  using UIntType = uint64_t;
};

template <typename T>
union FloatIEEE
{
  using FloatType = T;
  using IntType = typename FloatIEEETraits<T>::IntType;
  using UIntType = typename FloatIEEETraits<T>::UIntType;

  FloatType asFloat;
  IntType   asInt;
  UIntType  asUInt;

  FloatIEEE(FloatType f)
    : asFloat(f)
  {}
  FloatIEEE(IntType i)
    : asInt(i)
  {}
  bool
  Sign() const
  {
    return (asUInt >> (sizeof(asUInt) * 8 - 1)) != 0;
  }
  IntType
  AsULP() const
  {
    return this->Sign() ? IntType(~(~UIntType(0) >> 1) - asUInt) : asInt;
  }
};

} // end namespace Detail
} // end namespace Math

// move to itkConceptChecking?
namespace Concept
{
template <typename T>
struct FloatOrDouble;
template <>
struct FloatOrDouble<float>
{};
template <>
struct FloatOrDouble<double>
{};
} // end namespace Concept
} // end namespace itk

#undef USE_SSE2_32IMPL
#undef GCC_USE_ASM_32IMPL
#undef VC_USE_ASM_32IMPL

#undef USE_SSE2_64IMPL
#undef GCC_USE_ASM_64IMPL
#undef VC_USE_ASM_64IMPL

#endif // end of itkMathDetail.h
