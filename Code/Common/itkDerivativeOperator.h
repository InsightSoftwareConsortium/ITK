/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDerivativeOperator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

  =========================================================================*/
#ifndef __itkDerivativeOperator_h
#define __itkDerivativeOperator_h

#include "itkExceptionObject.h"
#include "itkNeighborhoodOperator.h"

namespace itk {

/**
 * \class DerivativeOperator
 * \brief A NeighborhoodOperator for taking an n-th order derivative
 * at a pixel
 * 
 * DerivativeOperator's coefficients are a tightest-fitting convolution
 * kernel for calculating the n-th order directional derivative at a pixel.
 * DerivativeOperator is a directional NeighborhoodOperator that should be
 * applied to a Neighborhood or NeighborhoodPointer using the inner product
 * method.
 *
 * \sa NeighborhoodOperator
 * \sa Neighborhood
 * \sa ForwardDifferenceOperator
 * \sa BackwardDifferenceOperator
 */
template<class TPixel, unsigned int VDimension=2>
class ITK_EXPORT DerivativeOperator
  : public NeighborhoodOperator<TPixel, VDimension>
{

public:
  /**
   * Run-time type information
   */
  itkTypeMacro(DerivativeOperator, NeighborhoodOperator);

  /**
   * Standard "Self" typedef support.
   */
  typedef DerivativeOperator Self;

  /**
   * NeighborhoodOperator typedef support.
   */
  typedef NeighborhoodOperator<TPixel, VDimension> NeighborhoodOperator;

  /**
   * Constructor
   */
  DerivativeOperator() : m_Order(1) {}

  /**
   * Sets the order of the derivative.
   */
  void SetOrder(const unsigned int &order)
  {
    m_Order = order;
  }

  /**
   * Returns the order of the derivative.
   */
  unsigned int GetOrder() const { return m_Order; }

  /**
   * Prints some debugging information
   */
  void PrintSelf()  // Note: This method is for devel/debugging
  {             // and should probably be removed at some point.
                //  jc 10-06-00
    NeighborhoodOperator::PrintSelf();
    std::cout << "DerivativeOperator" << std::endl;
    std::cout << "\tOrder = " << m_Order << std::endl;
  }
  
protected:
  typedef std::vector<TPixel> CoefficientVector;

  /**
   * Calculates operator coefficients.
   */
  CoefficientVector GenerateCoefficients();

  /**
   * Arranges coefficients spatially in the memory buffer.
   */
  void Fill(const CoefficientVector &coeff)
  {
    this->FillCenteredDirectional(coeff);
  }
 
private:
  /**
   * Order of the derivative.
   */
  unsigned int m_Order;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDerivativeOperator.txx"
#endif

#endif


