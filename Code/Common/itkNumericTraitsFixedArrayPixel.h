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


/** \class NumericTraits<FixedArray< T > >
 * \brief Define additional traits for FixedArray.
 *
 * We provide here a generic implementation based on creating types of
 * FixedArray whose components are the types of the NumericTraits from
 * the original FixedArray components. This implementation require
 * support for partial specializations, since it is based on the
 * concept that:
 *   NumericTraits<FixedArray< T > >  is defined piecewise by
 *   FixedArray< NumericTraits< T > >
 *
 * \sa NumbericTraits
 * \ingroup DataRepresentation
 */
template < typename T, unsigned int D >
class NumericTraits<FixedArray< T, D > >
{
private:

  typedef  typename NumericTraits<T>::AbsType        ElementAbsType;
  typedef  typename NumericTraits<T>::AccumulateType ElementAccumulateType;
  typedef  typename NumericTraits<T>::FloatType      ElementFloatType;
  typedef  typename NumericTraits<T>::PrintType      ElementPrintType;
  typedef  typename NumericTraits<T>::RealType       ElementRealType;

public:

  typedef T                                    ValueType;
  typedef FixedArray<T, D>                     Self;

  typedef FixedArray<ElementAbsType, D>        AbsType;
  typedef FixedArray<ElementAccumulateType, D> AccumulateType;
  typedef FixedArray<ElementFloatType, D>      FloatType;
  typedef FixedArray<ElementPrintType, D>      PrintType;
  typedef FixedArray<ElementRealType, D>       RealType;

  typedef ElementRealType                      ScalarRealType;

  static const Self max()
    {
      return Self( NumericTraits< T >::max() );
    }
  static const Self min()
    {
      return Self( NumericTraits< T >::min() );
    }
  static const Self NonpositiveMin()
    {
      return Self( NumericTraits< T >::NonpositiveMin() );
    }
  static const Self ZeroValue()
    {
      return Self( NumericTraits<T>::ZeroValue() );
    }
  static const Self OneValue()
    {
      return Self( NumericTraits<T>::OneValue() );
    }
  /// \note: the functions are prefered over the member variables as
  /// they are defined for all types
  static const Self ITKCommon_EXPORT Zero;
  static const Self ITKCommon_EXPORT One;
};


// a macro to define and initialize static member variables
#define itkStaticNumericTraitsGenericArrayMacro( GENERIC_ARRAY, T, D ) \
  template< > const GENERIC_ARRAY<T,D>  NumericTraits< GENERIC_ARRAY<T,D> >::Zero = GENERIC_ARRAY<T,D>( NumericTraits<T>::Zero ); \
  template< > const GENERIC_ARRAY<T,D>  NumericTraits< GENERIC_ARRAY<T,D> >::One = GENERIC_ARRAY<T,D>( NumericTraits<T>::One );


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


} // end namespace itk

#endif // __itkNumericTraitsFixedArrayPixel_h
