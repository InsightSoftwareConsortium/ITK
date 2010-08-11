/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsRGBAPixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNumericTraitsRGBAPixel_h
#define __itkNumericTraitsRGBAPixel_h

#include "itkNumericTraits.h"
#include "itkRGBAPixel.h"


namespace itk
{

// For all the other good compilers, we provide here a generic implementation
// based on creating types of RGBAPixels whose components are the types of the
// NumericTraits from the original RGBAPixel components. This implementation
// doesn't require specializations, since it is based on the concept that
//
//    NumericTraits< RGBAPixel< T > >  is defined piecewise by
//    RGBAPixel< NumericTraits< T > >
//
//
// By defining the following symbols, the Macro above gets customized to become
// a generic template implementation of the traits
//

template < typename T >
class NumericTraits<RGBAPixel< T > >
{
public:
  typedef T ValueType;

  typedef typename NumericTraits<T>::AbsType        ElementAbsType;
  typedef typename NumericTraits<T>::AccumulateType ElementAccumulateType;
  typedef typename NumericTraits<T>::FloatType      ElementFloatType;
  typedef typename NumericTraits<T>::PrintType      ElementPrintType;
  typedef typename NumericTraits<T>::RealType       ElementRealType;

  typedef RGBAPixel<T>                   Self;

  typedef RGBAPixel<ElementAbsType>          AbsType;
  typedef RGBAPixel<ElementAccumulateType>   AccumulateType;
  typedef RGBAPixel<ElementFloatType>        FloatType;
  typedef RGBAPixel<ElementPrintType>        PrintType;
  typedef RGBAPixel<ElementRealType>         RealType;

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
      return Self ( NumericTraits< ValueType >::NonpositiveMin() );
    }
  static const Self ZeroValue()
    {
      return Self( NumericTraits< T >::Zero );
    }
  static const Self OneValue()
    {
      return Self( NumericTraits< T >::One );
    }
  /// \note: the functions are prefered over the member variables as
  /// they are defined for all types
  static const Self ITKCommon_EXPORT Zero;
  static const Self ITKCommon_EXPORT One;
};


} // end namespace itk

#endif // __itkNumericTraitsRGBAPixel_h
