/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkForwardDifferenceOperator.txx
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
ForwardDifferenceOperator<TPixel, VDimension>
::GenerateCoefficients()
{
  std::vector<TPixel> coeff(3);

  coeff[0] = NumericTraits<ScalarValueType>::Zero;
  coeff[1] = -1.0f *  NumericTraits<ScalarValueType>::One;
  coeff[2] =  NumericTraits<ScalarValueType>::One;

  return coeff;
}
  
} // namespace itk
