/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDerivativeHalfForwardOperator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#ifndef __itkDerivativeHalfForwardOperator_h
#define __itkDerivativeHalfForwardOperator_h

#include "itkNeighborhoodOperator.h"
#include "itkExceptionObject.h"

namespace itk {

/**  
 * \class DerivativeHalfForwardOperator
 * \brief Operator whose inner product with a neighborhood returns
 * a "half" derivative at the center of the neighborhood.
 *
 * DerivativeHalfForwardOperator uses forward differences
 * i.e. F(x+1) - F(x) to calculate a "half" derivative useful, among
 * other things, in solving differential equations. It is a directional
 * NeighborhoodOperator that should be applied to a Neighborhood using the
 * inner product. 
 */
template<class TPixel, unsigned int VDimension=2>
class DerivativeHalfForwardOperator
  : public NeighborhoodOperator<TPixel, VDimension>
{
public:
  /**
   * Run-time type information
   */
  itkTypeMacro(DerivativeHalfForwardOperator, NeighborhoodOperator);

  /**
   * Standard "Self" typedef support.
   */
  typedef DerivativeHalfForwardOperator Self;

  /**
   * NeighborhoodOperator typedef support.
   */
  typedef NeighborhoodOperator<TPixel, VDimension> NeighborhoodOperator;

  /**
   * Constructor.
   */
  DerivativeHalfForwardOperator() {}

  /**
   * Required to support anonymous copying of NeighborhoodOperators.
   */
  NeighborhoodOperator *New() const { return new Self; }

  /**
   * Required to support anonymous copying of NeighborhoodOperators.
   */
  NeighborhoodOperator *Copy() const { return new Self(*this); }
  
protected:
  /**
   * Calculates operator coefficients.
   */
  std::vector<TPixel> GenerateCoefficients();
  
  /**
   * Arranges coefficients spatially in the memory buffer.
   */
  void Fill(const std::vector<TPixel> &coeff)
  {
    this->FillCenteredDirectional(coeff);
  }

};
  

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDerivativeHalfForwardOperator.txx"
#endif

#endif


