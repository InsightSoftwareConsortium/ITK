/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsDiffusionTensor3DPixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNumericTraitsDiffusionTensor3DPixel_h
#define __itkNumericTraitsDiffusionTensor3DPixel_h

#include "itkNumericTraitsFixedArrayPixel.h"
#include "itkDiffusionTensor3D.h"

// This file is meant to define numeric traits for tensor pixels types in itk

namespace itk
{

//
// First we define a macro that can be customized to be used for a sequence of
// specializations or for a generic template instantiation. This Macro covers
// the implementation for both partial specialization and total
// specializaion by defining certain macros.
//
#ifdef ITK_USE_NUMERIC_TRAITS_PARTIAL_SPECIALIZATION

#define itkNumericTraitsDiffusionTensor3DMacro(GENERIC_ARRAY, T)        \
  template < typename T >                                               \
  class NumericTraits<GENERIC_ARRAY< T > >                              \
  {                                                                     \
  private:                                                              \
                                                                        \
    typedef typename NumericTraits<T>::AbsType        ElementAbsType; \
    typedef typename NumericTraits<T>::AccumulateType ElementAccumulateType; \
    typedef typename NumericTraits<T>::FloatType      ElementFloatType; \
    typedef typename NumericTraits<T>::PrintType      ElementPrintType; \
    typedef typename NumericTraits<T>::RealType       ElementRealType; \
                                                                        \
  public:                                                               \
                                                                        \
    typedef T                                       ValueType;          \
    typedef GENERIC_ARRAY<T>                        Self;               \
                                                                        \
    typedef GENERIC_ARRAY<ElementAbsType>           AbsType;            \
    typedef GENERIC_ARRAY<ElementAccumulateType>    AccumulateType;     \
    typedef GENERIC_ARRAY<ElementFloatType>         FloatType;          \
    typedef GENERIC_ARRAY<ElementPrintType>         PrintType;          \
    typedef GENERIC_ARRAY<ElementRealType>          RealType;           \
                                                                        \
    typedef ElementRealType                         ScalarRealType;     \
                                                                        \
    static const Self max()                                             \
    {                                                                   \
      return Self( NumericTraits< T >::max() );                         \
    }                                                                   \
    static const Self min()                                             \
    {                                                                   \
      return Self( NumericTraits< T >::min() );                         \
    }                                                                   \
    static const Self NonpositiveMin()                                  \
    {                                                                   \
      return Self( NumericTraits< T >::NonpositiveMin() );              \
    }                                                                   \
    static const Self ZeroValue()                                       \
    {                                                                   \
      return Self( NumericTraits<T>::ZeroValue() );                     \
    }                                                                   \
    static const Self OneValue()                                        \
    {                                                                   \
      return Self( NumericTraits<T>::OneValue() );                      \
    }                                                                   \
    static const Self ITKCommon_EXPORT Zero;                            \
    static const Self ITKCommon_EXPORT One;                             \
};


itkNumericTraitsDiffusionTensor3DMacro(DiffusionTensor3D, T)

#else // ITK_USE_NUMERIC_TRAITS_PARTIAL_SPECIALIZATION

#define itkNumericTraitsDiffusionTensor3DMacro(GENERIC_ARRAY, T)        \
template< >                                                             \
  class NumericTraits<GENERIC_ARRAY< T > >                              \
  {                                                                     \
  private:                                                              \
                                                                        \
    typedef  NumericTraits<T>::AbsType        ElementAbsType; \
    typedef  NumericTraits<T>::AccumulateType ElementAccumulateType; \
    typedef  NumericTraits<T>::FloatType      ElementFloatType; \
    typedef  NumericTraits<T>::PrintType      ElementPrintType; \
    typedef  NumericTraits<T>::RealType       ElementRealType; \
                                                                        \
  public:                                                               \
                                                                        \
    typedef T                                       ValueType;          \
    typedef GENERIC_ARRAY<T>                        Self;               \
                                                                        \
    typedef GENERIC_ARRAY<ElementAbsType>           AbsType;            \
    typedef GENERIC_ARRAY<ElementAccumulateType>    AccumulateType;     \
    typedef GENERIC_ARRAY<ElementFloatType>         FloatType;          \
    typedef GENERIC_ARRAY<ElementPrintType>         PrintType;          \
    typedef GENERIC_ARRAY<ElementRealType>          RealType;           \
                                                                        \
    typedef ElementRealType                         ScalarRealType;     \
                                                                        \
    static const Self max()                                             \
    {                                                                   \
      return Self( NumericTraits< T >::max() );                         \
    }                                                                   \
    static const Self min()                                             \
    {                                                                   \
      return Self( NumericTraits< T >::min() );                         \
    }                                                                   \
    static const Self NonpositiveMin()                                  \
    {                                                                   \
      return Self( NumericTraits< T >::NonpositiveMin() );              \
    }                                                                   \
    static const Self ZeroValue()                                       \
    {                                                                   \
      return Self( NumericTraits<T>::ZeroValue() );                     \
    }                                                                   \
    static const Self OneValue()                                        \
    {                                                                   \
      return Self( NumericTraits<T>::OneValue() );                      \
    }                                                                   \
    static const Self ITKCommon_EXPORT Zero;                            \
    static const Self ITKCommon_EXPORT One;                             \
};

itkNumericTraitsDiffusionTensor3DMacro( DiffusionTensor3D, char );
itkNumericTraitsDiffusionTensor3DMacro( DiffusionTensor3D, unsigned char );
itkNumericTraitsDiffusionTensor3DMacro( DiffusionTensor3D, signed char );
itkNumericTraitsDiffusionTensor3DMacro( DiffusionTensor3D, short );
itkNumericTraitsDiffusionTensor3DMacro( DiffusionTensor3D, unsigned short );
itkNumericTraitsDiffusionTensor3DMacro( DiffusionTensor3D, int );
itkNumericTraitsDiffusionTensor3DMacro( DiffusionTensor3D, unsigned int );
itkNumericTraitsDiffusionTensor3DMacro( DiffusionTensor3D, long );
itkNumericTraitsDiffusionTensor3DMacro( DiffusionTensor3D, unsigned long );
itkNumericTraitsDiffusionTensor3DMacro( DiffusionTensor3D, float );
itkNumericTraitsDiffusionTensor3DMacro( DiffusionTensor3D, double );
itkNumericTraitsDiffusionTensor3DMacro( DiffusionTensor3D, long double );
#ifdef ITK_TYPE_USE_LONG_LONG
itkNumericTraitsDiffusionTensor3DMacro( DiffusionTensor3D, long long );
itkNumericTraitsDiffusionTensor3DMacro( DiffusionTensor3D, unsigned long long );
#endif // ITK_TYPE_USE_LONG_LONG

#endif // ITK_USE_NUMERIC_TRAITS_PARTIAL_SPECIALIZATION

} // end namespace itk

#endif // __itkNumericTraitsTensorPixel_h  
