/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkForwardDifferenceOperator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#ifndef __itkForwardDifferenceOperator_h
#define __itkForwardDifferenceOperator_h

#include "itkPixelTraits.h"
#include "itkNeighborhoodOperator.h"
#include "itkExceptionObject.h"

namespace itk {

/**  
 * \class ForwardDifferenceOperator
 * \brief Operator whose inner product with a neighborhood returns
 * a "half" derivative at the center of the neighborhood.
 *
 * ForwardDifferenceOperator uses forward differences
 * i.e. F(x+1) - F(x) to calculate a "half" derivative useful, among
 * other things, in solving differential equations. It is a directional
 * NeighborhoodOperator that should be applied to a Neighborhood using the
 * inner product. 
 */
template<class TPixel, unsigned int VDimension=2,
  class TAllocator = NeighborhoodAllocator<TPixel> >
class ITK_EXPORT ForwardDifferenceOperator
  : public NeighborhoodOperator<TPixel, VDimension, TAllocator>
{
public:
  /**
   * Run-time type information
   */
  itkTypeMacro(ForwardDifferenceOperator, NeighborhoodOperator);

  /**
   * Standard "Self" typedef support.
   */
  typedef ForwardDifferenceOperator Self;

  /**
   * Standard superclass typedef support
   */
  typedef NeighborhoodOperator<TPixel, VDimension, TAllocator> Superclass;

  /**
   * External support for scalar value type of the coefficient vector.
   */
  typedef typename ScalarTraits<TPixel>::ScalarValueType ScalarValueType;
  
  /**
   * Constructor.
   */
  ForwardDifferenceOperator() {}

  /**
   * Copy constructor
   */
  ForwardDifferenceOperator(const Self& other)
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
   * Necessary to work around VC++ compiler bug.
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
  {    this->FillCenteredDirectional(coeff);   }

};
  

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkForwardDifferenceOperator.txx"
#endif

#endif


