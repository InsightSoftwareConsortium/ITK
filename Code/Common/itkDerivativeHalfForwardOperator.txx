/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDerivativeHalfForwardOperator.txx
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
DerivativeHalfForwardOperator<TPixel, VDimension>
::GenerateCoefficients()
{
  std::vector<TPixel> coeff(3);
  ScalarTraits<TPixel>::SetScalar(coeff[0],
                                  NumericTraits<TPixelScalarValueType>::Zero);
  ScalarTraits<TPixel>::SetScalar(coeff[1], -1.0f *
                                  NumericTraits<TPixelScalarValueType>::One);
  ScalarTraits<TPixel>::SetScalar(coeff[2],
                                  NumericTraits<TPixelScalarValueType>::One);
  return coeff;
}
  
} // namespace itk
