/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNumericTraitsVariableLengthVectorPixel.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkNumericTraitsVariableLengthVectorPixel_h
#define __itkNumericTraitsVariableLengthVectorPixel_h

#include "itkNumericTraits.h"
#include "itkVariableLengthVector.h"

// This file defines numeric traits for VariableLengthVector< T > as pixel type
// Note that the Zero(), One(), min() and max() methods here take references to
// a pixel as input.  This is due to the fact that the length of the
// VariableLengthVector is not known until run-time. Since the most common use
// of Zero and One is for comparison purposes or initialization of sums etc,
// this might just as easily be re-written with a pixel passed in as a
// reference and the length is inferred from this pixel.
//
// This work is part of the National Alliance for Medical Image Computing
// (NAMIC), funded by the National Institutes of Health through the NIH Roadmap
// for Medical Research, Grant U54 EB005149.


namespace itk
{

//
// First we define a macro that can be customized to be used for a sequence of
// specializations or for a generic template instantiation. This Macro covers
// the implementation for good compilers and for Visual Studio 6.0.
//

template < typename T >
class NumericTraits<VariableLengthVector< T > >
{
public:
  typedef T ValueType;

  typedef typename NumericTraits<T>::AbsType        ElementAbsType;
  typedef typename NumericTraits<T>::AccumulateType ElementAccumulateType;
  typedef typename NumericTraits<T>::FloatType      ElementFloatType;
  typedef typename NumericTraits<T>::PrintType      ElementPrintType;
  typedef typename NumericTraits<T>::RealType       ElementRealType;

  typedef VariableLengthVector<T>                   Self;

  typedef VariableLengthVector<ElementAbsType>          AbsType;
  typedef VariableLengthVector<ElementAccumulateType>   AccumulateType;
  typedef VariableLengthVector<ElementFloatType>        FloatType;
  typedef VariableLengthVector<ElementPrintType>        PrintType;
  typedef VariableLengthVector<ElementRealType>         RealType;

  typedef ElementRealType ScalarRealType;

  static const Self max( const Self & a )
    {
      Self b(a.Size());
      b.Fill( NumericTraits< T >::max() );
      return b;
    }
  static const Self min( const Self & a )
    {
      Self b(a.Size());
      b.Fill( NumericTraits< T >::min() );
      return b;
    }
  static const Self Zero( const Self  & a )
  {
    Self b(a.Size());
    b.Fill( NumericTraits< T >::Zero );
    return b;
  }
  static const Self One( const Self & a )
  {
    Self b(a.Size());
    b.Fill( NumericTraits< T >::One );
    return b;
  }
};


} // end namespace itk

#endif // __itkNumericTraitsVariableLengthVector_h
