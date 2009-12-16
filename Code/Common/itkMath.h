/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMath.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itkMath_h
#define __itkMath_h


#include "itkConfigure.h"
#include "itkIntTypes.h"
#include "itkMathDetail.h"


namespace itk 
{
namespace Math 
{

// These constants originate from VXL's vnl_math.h. They have been
// moved here to improve visibility, and to ensure that the constants
// are available during compile time ( as opposed to static const
// member vaiables).

// \f[e] The base of the natural logarithm or Euler's number
static const double e                = 2.7182818284590452354;
// \f[ \log_2 e \f]
static const double log2e            = 1.4426950408889634074;
// \f[ \log_10 e \f]
static const double log10e           = 0.43429448190325182765;
// \f[ \log_e 2 \f]
static const double ln2              = 0.69314718055994530942;
// \f[ \log_e 10 \f]
static const double ln10             = 2.30258509299404568402;
// \f[ \pi ] 
static const double pi               = 3.14159265358979323846;
// \f[ \frac{\pi}{2} \f] 
static const double pi_over_2        = 1.57079632679489661923;
// \f[ \frac{\pi}{4} \f] 
static const double pi_over_4        = 0.78539816339744830962;
// \f[ \frac{1}{\pi} \f] 
static const double one_over_pi      = 0.31830988618379067154;
// \f[ \frac{2}{\pi} \f] 
static const double two_over_pi      = 0.63661977236758134308;
// \f[ \frac{2}{\sqrt{\pi}} \f] 
static const double two_over_sqrtpi  = 1.12837916709551257390;
// \f[ \frac{2}{\sqrt{2\pi}} \f] 
static const double one_over_sqrt2pi = 0.39894228040143267794;
// \f[ \sqrt{2} \f] 
static const double sqrt2            = 1.41421356237309504880;
// \f[ \sqrt{ \frac{1}{2}} \f]
static const double sqrt1_2          = 0.70710678118654752440;


#ifdef ITK_HAS_INT_64
// A useful macro to generate a template floating point to integer conversion
// templated on the return type and using either the 32 bit, the 64 bit or
// the vanilla version
#define itkTemplateFloatingToIntegerMacro(name)                         \
  template <typename TReturn,typename TInput>                           \
  inline TReturn name(TInput x)                                         \
  {                                                                     \
                                                                        \
    if (sizeof(TReturn) <= 4)                                           \
      {                                                                 \
      return static_cast<TReturn>(Detail::name##_32(x));                \
      }                                                                 \
    else if (sizeof(TReturn) <= 8)                                      \
      {                                                                 \
      return static_cast<TReturn>(Detail::name##_64(x));                \
      }                                                                 \
    else                                                                \
      {                                                                 \
      return static_cast<TReturn>(Detail::name##_base<TReturn,TInput>(x)); \
      }                                                                 \
   }
#else
#define itkTemplateFloatingToIntegerMacro(name)                         \
  template <typename TReturn,typename TInput>                           \
  inline TReturn name(TInput x)                                         \
  {                                                                     \
    if (sizeof(TReturn) <= 4)                                           \
      {                                                                 \
      return static_cast<TReturn>(Detail::name##_32(x));                \
      }                                                                 \
    else                                                                \
      {                                                                 \
      return static_cast<TReturn>(Detail::name##_base<TReturn,TInput>(x)); \
      }                                                                 \
   }
#endif // end ITK_HAS_INT_64

// \brief Define TReturn itk::Math::RoundHalfIntegerToEven<TReturn,TInput>(TInput x) 
// Round towards nearest integer
//
// \tparam TReturn a type convertable from integer types
// \tparam TInput must be float or double
//
//         halfway cases are rounded towards the nearest even integer, e.g.
//         RoundHalfIntegerToEven( 1.5) ==  2
//         RoundHalfIntegerToEven(-1.5) == -2
//         RoundHalfIntegerToEven( 2.5) ==  2
//         RoundHalfIntegerToEven( 3.5) ==  4
//
// The behavior of overflow is undefined due to numerous implementations.
//
// \warning We assume that the rounding mode is not changed from the default
// one (or at least that it is always restored to the default one).
itkTemplateFloatingToIntegerMacro(RoundHalfIntegerToEven);

// \brief Define TReturn itk::Math::RoundHalfIntegerUp<TReturn,TInput>(TInput x) 
//  Round towards nearest integer
//
//         halfway cases are rounded upward, e.g.
//         RoundHalfIntegerUp( 1.5) ==  2
//         RoundHalfIntegerUp(-1.5) == -1
//         RoundHalfIntegerUp( 2.5) ==  3
//
// The behavior of overflow is undefined due to numerous implementations.
//
// \warning argument absolute value must be less than INT_MAX/2
// for RoundHalfIntegerUp to be guaranteed to work.
// We also assume that the rounding mode is not changed from the default
// one (or at least that it is always restored to the default one).
itkTemplateFloatingToIntegerMacro(RoundHalfIntegerUp);

// \brief Define TReturn itk::Math::Round<TReturn,TInput>(TInput x) 
// Round towards nearest integer (This is a synonym for RoundHalfIntegerUp)
//
//         halfway cases are rounded upward, e.g.
//         RoundHalfIntegerUp( 1.5) ==  2
//         RoundHalfIntegerUp(-1.5) == -1
//         RoundHalfIntegerUp( 2.5) ==  3
//
// The behavior of overflow is undefined due to numerous implementations.
//
// \warning argument absolute value must be less than INT_MAX/2
// for RoundHalfIntegerUp to be guaranteed to work.
// We also assume that the rounding mode is not changed from the default
// one (or at least that it is always restored to the default one).
//
// \sa RoundHalfIntegerUp
template <typename TReturn, typename TInput>
inline TReturn Round(TInput x) { return RoundHalfIntegerUp<TReturn,TInput>(x); }

// \brief Define TReturn itk::Math::Floor<TReturn,TInput>(TInput x) 
// Round towards minus infinity
//
// The behavior of overflow is undefined due to numerous implementations.
//
// \warning argument absolute value must be less than INT_MAX/2
// for vnl_math_floor to be guaranteed to work.
// We also assume that the rounding mode is not changed from the default
// one (or at least that it is always restored to the default one).
itkTemplateFloatingToIntegerMacro(Floor);

// \brief Define TReturn itk::Math::Ceil<TReturn,TInput>(TInput x)
// Round towards plus infinity
//
// The behavior of overflow is undefined due to numerous implementations.
//
// \warning argument absolute value must be less than INT_MAX/2
// for vnl_math_ceil to be guaranteed to work.
// We also assume that the rounding mode is not changed from the default
// one (or at least that it is always restored to the default one)
itkTemplateFloatingToIntegerMacro(Ceil);


#undef  itkTemplateFloatingToIntegerMacro


#if !defined(ITK_LEGACY_REMOVE) &&  !VCL_TEMPLATE_MATCHES_TOO_OFTEN
// VCL_TEMPLATE_MATCHES_TOO_OFTEN is used here because some compilers
// can not handle function overloading with templated and
// non-templated methods, ie the templated functions matches too often

/**
 * These methods have been deprecated as of ITK 3.16
 * Please use the templated method
 * TReturn itk::Math::XXX<TReturn,TInput>(TInput x) instead.
 */

inline int RoundHalfIntegerToEven(double x) { return Detail::RoundHalfIntegerToEven_32(x); }
inline int RoundHalfIntegerToEven(float  x) { return Detail::RoundHalfIntegerToEven_32(x); }

inline int RoundHalfIntegerUp(double x) { return Detail::RoundHalfIntegerUp_32(x); }
inline int RoundHalfIntegerUp(float  x) { return Detail::RoundHalfIntegerUp_32(x); }

inline int Round(double x) { return Detail::RoundHalfIntegerUp_32(x); }
inline int Round(float  x) { return Detail::RoundHalfIntegerUp_32(x); }

inline int Floor(double x) { return Detail::Floor_32(x); }
inline int Floor(float  x) { return Detail::Floor_32(x); }

inline int Ceil(double x) { return Detail::Ceil_32(x); }
inline int Ceil(float  x) { return Detail::Ceil_32(x); }

#endif // end of ITK_LEGACY_REMOVE

template <typename TReturn,typename TInput>
inline TReturn CastWithRangeCheck(TInput x)
{
  return static_cast<TReturn>( x );
}

} // end namespace Math
} // end namespace itk
#endif // end of itkMath.h
