/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDerivativeHalfBackwardOperator.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
namespace itk
{

template <class TPixel, unsigned int VDimension>
std::vector<TPixel>
DerivativeHalfBackwardOperator<TPixel, VDimension>
::GenerateCoefficients()
{
  std::vector<TPixel> coeff(3);
  coeff[0] = -1.0f *  NumericTraits<TPixelScalarValueType>::One;
  coeff[1] =  NumericTraits<TPixelScalarValueType>::One;
  coeff[2] =  NumericTraits<TPixelScalarValueType>::Zero;

  return coeff;
}
  
} // namespace itk
