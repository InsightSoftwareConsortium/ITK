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
#include "vnl/vnl_math.h"
#include "itkMathDetail.h"

// note: including Macro.h here will need some thought, which comes first?

namespace itk 
{
namespace Math 
{


// A useful macro to generate a template floating point to integer conversion
// templated on the return type and using either the 32 bit, the 64 bit or
// the vanilla version
#ifdef VXL_HAS_INT_64
#define itkTemplateFloatingToIntegerMacro(name)                         \
  template <typename TReturn,typename TInput>                           \
  inline TReturn name(TInput x)                                         \
  {                                                                     \
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
#endif

/** Define TReturn itk::Math::RoundHalfIntegerToEven<TReturn,TInput>(TInput x) */
itkTemplateFloatingToIntegerMacro(RoundHalfIntegerToEven);

/** Define TReturn itk::Math::RoundHalfIntegerUp<TReturn,TInput>(TInput x) */
itkTemplateFloatingToIntegerMacro(RoundHalfIntegerUp);

/** Define TReturn itk::Math::Round<TReturn,TInput>(TInput x) */
template <typename TReturn, typename TInput>
inline TReturn Round(TInput x) { return RoundHalfIntegerUp<TReturn,TInput>(x); }

/** Define TReturn itk::Math::Floor<TReturn,TInput>(TInput x) */
itkTemplateFloatingToIntegerMacro(Floor);

/** Define TReturn itk::Math::Ceil<TReturn,TInput>(TInput x) */
itkTemplateFloatingToIntegerMacro(Ceil);


#undef  itkTemplateFloatingToIntegerMacro

#ifndef ITK_LEGACY_REMOVE
/**
 * These methods have been deprecated as of ITK 3.16
 * Please use the templated method
 * TReturn itk::Math::XXX<TReturn,TInput>(TInput x) instead.
 */

inline int RoundHalfIntegerToEven(double x) { return Detail::RoundHalfIntegerToEven_32(x); }
inline int RoundHalfIntegerToEven(float  x) { return Detail::RoundHalfIntegerToEven_32(x); }

inline int RoundHalfIntegerUp(double x) { return Detail::RoundHalfIntegerUp_32(x); }
inline int RoundHalfIntegerUp(float  x) { return Detail::RoundHalfIntegerUp_32(x); }

inline int Round(double x) { return RoundHalfIntegerUp(x); }
inline int Round(float  x) { return RoundHalfIntegerUp(x); }

inline int Floor(double x) { return Detail::Floor_32(x); }
inline int Floor(float  x) { return Detail::Floor_32(x); }

inline int Ceil(double x) { return Detail::Ceil_32(x); }
inline int Ceil(float  x) { return Detail::Ceil_32(x); }

#endif // end of ITK_LEGACY_REMOVE

#undef TemplateFloatingToInteger


} // end namespace Math
} // end namespace itk
#endif // end of itkMath.h
