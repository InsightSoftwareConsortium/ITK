/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBackwardDifferenceOperator.txx
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
typename BackwardDifferenceOperator<TPixel, VDimension>::CoefficientVector
BackwardDifferenceOperator<TPixel, VDimension>
::GenerateCoefficients()
{
  CoefficientVector coeff(3);
  coeff[0] = -1.0f *  NumericTraits<ScalarValueType>::One;
  coeff[1] =  NumericTraits<ScalarValueType>::One;
  coeff[2] =  NumericTraits<ScalarValueType>::Zero;

  return coeff;
}
  
} // namespace itk
