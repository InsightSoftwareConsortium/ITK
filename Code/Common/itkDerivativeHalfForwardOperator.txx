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

template <class TDataType, unsigned int VDimension>
std::vector<TDataType>
DerivativeHalfForwardOperator<TDataType, VDimension>
::GenerateCoefficients()
{
  std::vector<TDataType> coeff(3);
  coeff[0] =  0.0f;
  coeff[1] = -1.0f;
  coeff[2] =  1.0f;
  return coeff;
}
  
} // namespace itk
