/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDerivativeHalfBackwardOperator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#ifndef __itkDerivativeHalfBackwardOperator_h
#define __itkDerivativeHalfBackwardOperator_h

#include "itkNeighborhoodOperator.h"
#include "itkExceptionObject.h"

namespace itk {

/**
 * \class DerivativeHalfBackwardOperator
 * \brief Operator whose inner product with a neighborhood returns
 * a "half" derivative at the center of the neighborhood.
 *
 * DerivativeHalfBackwardOperator uses backward differences
 * i.e. F(x) - F(x-1) to calculate a "half" derivative useful, among
 * other things, in solving differential equations. It is a directional
 * NeighborhoodOperator that should be applied to a Neighborhood using the
 * inner product. 
 *
 */
template<class TPixel, unsigned int VDimension=2>
class DerivativeHalfBackwardOperator
  : public NeighborhoodOperator<TPixel, VDimension>
{
public:
  /**
   * Run-time type information
   */
  itkTypeMacro(DerivativeHalfBackwardOperator, NeighborhoodOperator);

  /**
   * Standard "Self" typedef support.
   */
  typedef DerivativeHalfBackwardOperator Self;

  /**
   * NeighborhoodOperator typedef support.
   */
  typedef NeighborhoodOperator<TPixel, VDimension> NeighborhoodOperator;
  
  /**
   *  Constructor.
   */
  DerivativeHalfBackwardOperator() {}

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
#include "itkDerivativeHalfBackwardOperator.txx"
#endif

#endif


