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

#include "itkNumericTraits.h"
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

template < typename T >
class NumericTraits< DiffusionTensor3D< T > >
{
private:

  typedef typename NumericTraits<T>::AbsType        ElementAbsType;
  typedef typename NumericTraits<T>::AccumulateType ElementAccumulateType;
  typedef typename NumericTraits<T>::FloatType      ElementFloatType;
  typedef typename NumericTraits<T>::PrintType      ElementPrintType;
  typedef typename NumericTraits<T>::RealType       ElementRealType;

public:

  typedef T                                       ValueType;
  typedef DiffusionTensor3D<T>                    Self;

  typedef DiffusionTensor3D<ElementAbsType>         AbsType;
  typedef DiffusionTensor3D<ElementAccumulateType>  AccumulateType;
  typedef DiffusionTensor3D<ElementFloatType>       FloatType;
  typedef DiffusionTensor3D<ElementPrintType>       PrintType;
  typedef DiffusionTensor3D<ElementRealType>        RealType;

  typedef ElementRealType                         ScalarRealType;

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


} // end namespace itk

#endif // __itkNumericTraitsTensorPixel_h
