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
  coeff[0] = -1.0f *  NumericTraits<ScalarValueType>::One;
  coeff[1] =  NumericTraits<ScalarValueType>::One;
  coeff[2] =  NumericTraits<ScalarValueType>::Zero;

  return coeff;
}
  
} // namespace itk

#endif
