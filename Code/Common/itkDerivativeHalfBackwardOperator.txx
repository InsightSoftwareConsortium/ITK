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
  // Only sets the scalar component of the data.  This works fine for native
  // types and for itk::Scalar<>.
  std::vector<TPixel> coeff(3);
  ScalarTraits<TPixel>::SetScalar(coeff[0], -1.0f *
                                  NumericTraits<TPixelScalarValueType>::One);
  ScalarTraits<TPixel>::SetScalar(coeff[1],
                                  NumericTraits<TPixelScalarValueType>::One);
  ScalarTraits<TPixel>::SetScalar(coeff[2],
                                  NumericTraits<TPixelScalarValueType>::Zero);
  return coeff;
}
  
} // namespace itk
