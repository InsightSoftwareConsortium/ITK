/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkForwardDifferenceOperator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkForwardDifferenceOperator_txx
#define _itkForwardDifferenceOperator_txx
#include "itkForwardDifferenceOperator.h"

#include "itkNumericTraits.h"

namespace itk
{

template <class TPixel, unsigned int VDimension, class TAllocator>
typename ForwardDifferenceOperator<TPixel, VDimension, TAllocator>
::CoefficientVector
ForwardDifferenceOperator<TPixel, VDimension, TAllocator>
::GenerateCoefficients()
{
  CoefficientVector coeff(3);

  coeff[0] = NumericTraits<PixelType>::Zero;
  coeff[1] = -1.0f *  NumericTraits<PixelType>::One;
  coeff[2] =  NumericTraits<PixelType>::One;

  return coeff;
}
  
} // namespace itk

#endif
