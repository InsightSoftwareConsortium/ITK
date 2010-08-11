/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsRGBPixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNumericTraitsRGBPixel_h
#define __itkNumericTraitsRGBPixel_h

#include "itkNumericTraits.h"
#include "itkRGBPixel.h"


namespace itk
{

//
// First we define a macro that can be customized to be used for a sequence of
// specializations or for a generic template instantiation. This Macro covers
// the implementation for good compilers and for Visual Studio 6.0.
//

// For all the other good compilers, we provide here a generic implementation
// based on creating types of RGBPixels whose components are the types of the
// NumericTraits from the original RGBPixel components. This implementation
// doesn't require specializations, since it is based on the concept that
//
//    NumericTraits< RGBAPixle< T > >  is defined piecewise by
//    RGBAPixle< NumericTraits< T > >
//
//
// By defining the following symbols, the Macro above gets customized to become
// a generic template implementation of the traits
//

template < typename T >
class NumericTraits<RGBPixel< T > >
{
public:
  typedef T ValueType;

  typedef typename NumericTraits<T>::AbsType        ElementAbsType;
  typedef typename NumericTraits<T>::AccumulateType ElementAccumulateType;
  typedef typename NumericTraits<T>::FloatType      ElementFloatType;
  typedef typename NumericTraits<T>::PrintType      ElementPrintType;
  typedef typename NumericTraits<T>::RealType       ElementRealType;

  typedef RGBPixel<T>                   Self;

  typedef RGBPixel<ElementAbsType>          AbsType;
  typedef RGBPixel<ElementAccumulateType>   AccumulateType;
  typedef RGBPixel<ElementFloatType>        FloatType;
  typedef RGBPixel<ElementPrintType>        PrintType;
  typedef RGBPixel<ElementRealType>         RealType;

  typedef ElementRealType ScalarRealType;

  static const Self max( const Self & )
    {
      return Self( NumericTraits< T >::max() );
    }
  static const Self min( const Self & )
    {
      return Self( NumericTraits< T >::min() );
    }
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
      return Self( NumericTraits< ValueType >::NonpositiveMin() );
    }
  static const Self ZeroValue()
  {
    return Self( NumericTraits< T >::Zero );
  }
  static const Self OneValue()
  {
    return Self( NumericTraits< T >::One );
  }
  static const Self ITKCommon_EXPORT Zero;
  static const Self ITKCommon_EXPORT One;
};


} // end namespace itk

#endif // __itkNumericTraitsRGBPixel_h
