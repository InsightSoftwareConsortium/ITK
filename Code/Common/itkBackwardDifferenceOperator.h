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

#include "itkPixelTraits.h"
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
template<class TPixel, unsigned int VDimension=2,
  class TAllocator = NeighborhoodAllocator<TPixel> >
class ITK_EXPORT BackwardDifferenceOperator
  : public NeighborhoodOperator<TPixel, VDimension, TAllocator>
{
public:
  /**
   * Standard "Self" typedef support.
   */
  typedef BackwardDifferenceOperator Self;
  
  /**
   * Standard Superclass typedef support
   */
  typedef NeighborhoodOperator<TPixel, VDimension, TAllocator> Superclass;
  
  /**
   * External support for coefficient type
   */
  typedef typename ScalarTraits<TPixel>::ScalarValueType ScalarValueType;
  
  /**
   * Constructor.
   */
  BackwardDifferenceOperator() {}

  /**
   * Copy constructor
   */
  BackwardDifferenceOperator(const Self& other)
    : NeighborhoodOperator<TPixel, VDimension, TAllocator>(other)
  {  }

  /**
   * Assignment operator
   */
  Self &operator=(const Self& other)
  {
    Superclass::operator=(other);
    return *this;
  }

protected:
  /**
   * Necessary to work around a compiler bug in VC++.
   */
  typedef typename Superclass::CoefficientVector CoefficientVector;

  /**
   * Calculates operator coefficients.
   */
  CoefficientVector GenerateCoefficients();

  /**
   * Arranges coefficients spatially in the memory buffer.
   */
  void Fill(const CoefficientVector &coeff)
  {    this->FillCenteredDirectional(coeff);  }
  
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkBackwardDifferenceOperator.txx"
#endif

#endif


