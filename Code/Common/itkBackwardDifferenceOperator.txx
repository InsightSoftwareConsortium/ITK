/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBackwardDifferenceOperator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkBackwardDifferenceOperator_txx
#define _itkBackwardDifferenceOperator_txx
namespace itk
{

template <class TPixel, unsigned int VDimension, class TAllocator>
typename BackwardDifferenceOperator<TPixel, VDimension, TAllocator>
::CoefficientVector
BackwardDifferenceOperator<TPixel, VDimension, TAllocator>
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
