/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsFixedArrayPixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNumericTraitsFixedArrayPixel_h
#define __itkNumericTraitsFixedArrayPixel_h

#include "itkNumericTraits.h"
#include "itkFixedArray.h"


namespace itk
{


//
// First we define a macro that can be customized to be used for a sequence of
// specializations or for a generic template instantiation. This Macro covers
// the implementation for both partial specialization and total
// specializaion by defining certain macros.
//
#define itkNumericTraitsGenericArrayMacro(GENERIC_ARRAY, T, D)          \
  template < ITK_NUMERIC_TRAITS_TEMPLATE_ARGUMENTS >                    \
  class NumericTraits<GENERIC_ARRAY< T, D > >                           \
  {                                                                     \
  private:                                                              \
                                                                        \
    typedef  ITK_NUMERIC_TRAITS_TYPENAME NumericTraits<T>::AbsType        ElementAbsType; \
    typedef  ITK_NUMERIC_TRAITS_TYPENAME NumericTraits<T>::AccumulateType ElementAccumulateType; \
    typedef  ITK_NUMERIC_TRAITS_TYPENAME NumericTraits<T>::FloatType      ElementFloatType; \
    typedef  ITK_NUMERIC_TRAITS_TYPENAME NumericTraits<T>::PrintType      ElementPrintType; \
    typedef  ITK_NUMERIC_TRAITS_TYPENAME NumericTraits<T>::RealType       ElementRealType; \
                                                                        \
  public:                                                               \
                                                                        \
    typedef T                                       ValueType;          \
    typedef GENERIC_ARRAY<T, D>                     Self;               \
                                                                        \
    typedef GENERIC_ARRAY<ElementAbsType, D>        AbsType;            \
    typedef GENERIC_ARRAY<ElementAccumulateType, D> AccumulateType;     \
    typedef GENERIC_ARRAY<ElementFloatType, D>      FloatType;          \
    typedef GENERIC_ARRAY<ElementPrintType, D>      PrintType;          \
    typedef GENERIC_ARRAY<ElementRealType, D>       RealType;           \
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


#ifdef ITK_USE_NUMERIC_TRAITS_PARTIAL_SPECIALIZATION


// For all the good compilers, we provide here a generic implementation
// based on creating types of FixedArray whose components are the types of the
// NumericTraits from the original RGBAPixel components. This implementation
// require support for partial specializations, since it is based on
// the concept that:
//
//    NumericTraits<FixedArray< T > >  is defined piecewise by
//    FixedArray< NumericTraits< T > >
//
#define ITK_NUMERIC_TRAITS_TYPENAME typename
#define ITK_NUMERIC_TRAITS_TEMPLATE_ARGUMENTS   typename T, unsigned int D


#define itkNumericTraitsGenericArrayScalarsDimensionsMacro( GENERIC_ARRAY ) \
  itkNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, D );

// a macro to define and initialize static member variables
#define itkStaticNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, D )  \
  template< > const GENERIC_ARRAY<T,D>  NumericTraits< GENERIC_ARRAY<T,D> >::Zero = GENERIC_ARRAY<T,D>( NumericTraits<T>::Zero ); \
  template< > const GENERIC_ARRAY<T,D>  NumericTraits< GENERIC_ARRAY<T,D> >::One = GENERIC_ARRAY<T,D>( NumericTraits<T>::One );


#else // ITK_USE_NUMERICTRAITS_PARTIAL_SPECIALIZATION

// For the "bad" compilers we need total specialization of the
// NumericTraits. That means over the the types and the dimension of
// the array. We add a macro to instantiate the NumericTrait macro over
// only the dimensions. Then manually instantiated for all they type,
// which may have compile time conditionals.


// These two symbols below are defined empty on purpose
#define ITK_NUMERIC_TRAITS_TYPENAME
#define ITK_NUMERIC_TRAITS_TEMPLATE_ARGUMENTS 

//
// List here the array dimension specializations of these Traits:
//
#define itkNumericTraitsGenericArrayDimensionsMacro( GENERIC_ARRAY, T ) \
  itkNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, 1 );             \
  itkNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, 2 );             \
  itkNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, 3 );             \
  itkNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, 4 );             \
  itkNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, 5 );             \
  itkNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, 6 );             \
  itkNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, 7 );             \
  itkNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, 8 );             \
  itkNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, 9 );             \
  itkNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, 10 );

// a macro to define and initialize static member variables
#define itkStaticNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, D )  \
  const GENERIC_ARRAY<T,D>  NumericTraits< GENERIC_ARRAY<T,D> >::Zero = GENERIC_ARRAY<T,D>( NumericTraits<T>::Zero ); \
  const GENERIC_ARRAY<T,D>  NumericTraits< GENERIC_ARRAY<T,D> >::One = GENERIC_ARRAY<T,D>( NumericTraits<T>::One );

#endif // ITK_USE_NUMERICTRAITS_PARTIAL_SPECIALIZATION


//
// List here the array dimension specializations of these static
// Traits: 
//
#define itkStaticNumericTraitsGenericArrayDimensionsMacro( GENERIC_ARRAY, T ) \
  itkStaticNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, 1 ); \
  itkStaticNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, 2 ); \
  itkStaticNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, 3 ); \
  itkStaticNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, 4 ); \
  itkStaticNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, 5 ); \
  itkStaticNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, 6 ); \
  itkStaticNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, 7 ); \
  itkStaticNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, 8 ); \
  itkStaticNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, 9 ); \
  itkStaticNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, 10 );


//
// Instantiate the macros to declare the NumericTraits for the
// FixedArray types.
//
#ifdef ITK_USE_NUMERIC_TRAITS_PARTIAL_SPECIALIZATION

itkNumericTraitsGenericArrayScalarsDimensionsMacro( FixedArray );

#else // ITK_USE_NUMERIC_TRAITS_PARTIAL_SPECIALIZATION

itkNumericTraitsGenericArrayDimensionsMacro( FixedArray, char );
itkNumericTraitsGenericArrayDimensionsMacro( FixedArray, unsigned char );
itkNumericTraitsGenericArrayDimensionsMacro( FixedArray, signed char );
itkNumericTraitsGenericArrayDimensionsMacro( FixedArray, short );
itkNumericTraitsGenericArrayDimensionsMacro( FixedArray, unsigned short );
itkNumericTraitsGenericArrayDimensionsMacro( FixedArray, int );
itkNumericTraitsGenericArrayDimensionsMacro( FixedArray, unsigned int );
itkNumericTraitsGenericArrayDimensionsMacro( FixedArray, long );
itkNumericTraitsGenericArrayDimensionsMacro( FixedArray, unsigned long );
itkNumericTraitsGenericArrayDimensionsMacro( FixedArray, float );
itkNumericTraitsGenericArrayDimensionsMacro( FixedArray, double );
itkNumericTraitsGenericArrayDimensionsMacro( FixedArray, long double );
#ifdef ITK_TYPE_USE_LONG_LONG
itkNumericTraitsGenericArrayDimensionsMacro( FixedArray, long long );
itkNumericTraitsGenericArrayDimensionsMacro( FixedArray, unsigned long long );
#endif // ITK_TYPE_USE_LONG_LONG

#endif // ITK_USE_NUMERIC_TRAITS_PARTIAL_SPECIALIZATION

} // end namespace itk

#endif // __itkNumericTraitsFixedArrayPixel_h
