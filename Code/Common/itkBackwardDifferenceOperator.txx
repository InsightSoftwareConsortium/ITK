/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBackwardDifferenceOperator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkBackwardDifferenceOperator_txx
#define __itkBackwardDifferenceOperator_txx

#include "itkBackwardDifferenceOperator.h"

namespace itk
{

template <class TPixel, unsigned int TDimension, class TAllocator>
typename BackwardDifferenceOperator<TPixel, TDimension, TAllocator>
::CoefficientVector
BackwardDifferenceOperator<TPixel, TDimension, TAllocator>
::GenerateCoefficients()
{
  CoefficientVector coeff(3);
  coeff[0] = -1.0f *  NumericTraits<PixelType>::One;
  coeff[1] =  NumericTraits<PixelType>::One;
  coeff[2] =  NumericTraits<PixelType>::Zero;

  return coeff;
}
  
} // namespace itk

#endif
