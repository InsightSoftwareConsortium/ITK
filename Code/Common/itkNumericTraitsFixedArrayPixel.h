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
// the implementation for good compilers and for Visual Studio 6.0.
//
#define itkNumericTraitsGenericArrayMacro(GenericArray, T, D) \
template < >  \
class NumericTraits<GenericArray< T, D > >  \
{ \
public: \
  typedef T ValueType; \
 \
  typedef  NumericTraits<T>::AbsType        ElementAbsType; \
  typedef  NumericTraits<T>::AccumulateType ElementAccumulateType; \
  typedef  NumericTraits<T>::FloatType      ElementFloatType; \
  typedef  NumericTraits<T>::PrintType      ElementPrintType; \
  typedef  NumericTraits<T>::RealType       ElementRealType; \
 \
  typedef GenericArray<T, D>                          Self; \
 \
  typedef GenericArray<ElementAbsType, D>             AbsType; \
  typedef GenericArray<ElementAccumulateType, D>      AccumulateType; \
  typedef GenericArray<ElementFloatType, D>           FloatType; \
  typedef Self                                        PrintType; \
  typedef GenericArray<ElementRealType, D>            RealType; \
 \
  typedef ElementRealType                             ScalarRealType; \
 \
  static const Self max() \
    {  \
      Self b;  \
      b.Fill( NumericTraits< T >::max() ); \
      return b; \
    } \
  static const Self min() \
    {  \
      Self b;  \
      b.Fill( NumericTraits< T >::min() ); \
      return b; \
    } \
  static const Self NonpositiveMin() \
    {  \
      return NumericTraits< Self >::min(); \
    } \
  static const Self ZeroValue() \
  {  \
    return NumericTraits< GenericArray< T, D > >::Zero; \
  } \
  static const Self OneValue() \
  {  \
    return NumericTraits< GenericArray< T, D > >::One; \
  } \
  static const Self ITKCommon_EXPORT Zero; \
  static const Self ITKCommon_EXPORT One; \
};


#define itkNumericTraitsGenericArrayScalarsMacro( GenericArray, D ) \
itkNumericTraitsGenericArrayMacro( GenericArray, char, D ); \
itkNumericTraitsGenericArrayMacro( GenericArray, unsigned char, D ); \
itkNumericTraitsGenericArrayMacro( GenericArray, signed char, D ); \
itkNumericTraitsGenericArrayMacro( GenericArray, short, D ); \
itkNumericTraitsGenericArrayMacro( GenericArray, unsigned short, D ); \
itkNumericTraitsGenericArrayMacro( GenericArray, int, D ); \
itkNumericTraitsGenericArrayMacro( GenericArray, unsigned int, D ); \
itkNumericTraitsGenericArrayMacro( GenericArray, long, D ); \
itkNumericTraitsGenericArrayMacro( GenericArray, unsigned long, D ); \
itkNumericTraitsGenericArrayMacro( GenericArray, float, D ); \
itkNumericTraitsGenericArrayMacro( GenericArray, double, D ); \
itkNumericTraitsGenericArrayMacro( GenericArray, long double, D ); \


#define itkStaticNumericTraitsGenericArrayMacro( GenericArray, T, D ) \
const GenericArray<T,D>  NumericTraits< GenericArray<T,D> >::Zero = GenericArray<T,D>( NumericTraits<T>::Zero ); \
const GenericArray<T,D>  NumericTraits< GenericArray<T,D> >::One = GenericArray<T,D>( NumericTraits<T>::One ); \

#define itkStaticNumericTraitsGenericArrayScalarsMacro( GenericArray, D ) \
itkStaticNumericTraitsGenericArrayMacro( GenericArray, unsigned char, D ); \
itkStaticNumericTraitsGenericArrayMacro( GenericArray, signed char, D ); \
itkStaticNumericTraitsGenericArrayMacro( GenericArray, char, D ); \
itkStaticNumericTraitsGenericArrayMacro( GenericArray, short, D ); \
itkStaticNumericTraitsGenericArrayMacro( GenericArray, unsigned short, D ); \
itkStaticNumericTraitsGenericArrayMacro( GenericArray, int, D ); \
itkStaticNumericTraitsGenericArrayMacro( GenericArray, unsigned int, D ); \
itkStaticNumericTraitsGenericArrayMacro( GenericArray, long, D ); \
itkStaticNumericTraitsGenericArrayMacro( GenericArray, unsigned long, D ); \
itkStaticNumericTraitsGenericArrayMacro( GenericArray, float, D ); \
itkStaticNumericTraitsGenericArrayMacro( GenericArray, double, D ); \
itkStaticNumericTraitsGenericArrayMacro( GenericArray, long double, D ); \


itkNumericTraitsGenericArrayScalarsMacro( FixedArray, 1);
itkNumericTraitsGenericArrayScalarsMacro( FixedArray, 2);
itkNumericTraitsGenericArrayScalarsMacro( FixedArray, 3);
itkNumericTraitsGenericArrayScalarsMacro( FixedArray, 4);
itkNumericTraitsGenericArrayScalarsMacro( FixedArray, 5);
itkNumericTraitsGenericArrayScalarsMacro( FixedArray, 6);
itkNumericTraitsGenericArrayScalarsMacro( FixedArray, 7);
itkNumericTraitsGenericArrayScalarsMacro( FixedArray, 8);
itkNumericTraitsGenericArrayScalarsMacro( FixedArray, 9);
itkNumericTraitsGenericArrayScalarsMacro( FixedArray, 10);


} // end namespace itk

#endif // __itkNumericTraitsGenericArrayPixel_h
