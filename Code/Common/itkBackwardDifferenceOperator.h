/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkBackwardDifferenceOperator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#ifndef __itkBackwardDifferenceOperator_h
#define __itkBackwardDifferenceOperator_h

#include "itkNeighborhoodOperator.h"
#include "itkExceptionObject.h"

namespace itk {

/**
 * \class BackwardDifferenceOperator
 * \brief Operator whose inner product with a neighborhood returns
 * a "half" derivative at the center of the neighborhood.
 *
 * BackwardDifferenceOperator uses backward differences
 * i.e. F(x) - F(x-1) to calculate a "half" derivative useful, among
 * other things, in solving differential equations. It is a directional
 * NeighborhoodOperator that should be applied to a Neighborhood using the
 * inner product. 
 *
 */
template<class TPixel, unsigned int VDimension=2>
class BackwardDifferenceOperator
  : public NeighborhoodOperator<TPixel, VDimension>
{
public:
  /**
   * Run-time type information
   */
  itkTypeMacro(BackwardDifferenceOperator, NeighborhoodOperator);

  /**
   * Standard "Self" typedef support.
   */
  typedef BackwardDifferenceOperator Self;

  /**
   * NeighborhoodOperator typedef support.
   */
  typedef NeighborhoodOperator<TPixel, VDimension> NeighborhoodOperator;
  
  /**
   *  Constructor.
   */
  BackwardDifferenceOperator() {}

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
#include "itkBackwardDifferenceOperator.txx"
#endif

#endif


