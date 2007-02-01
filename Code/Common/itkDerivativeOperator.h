/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkDerivativeOperator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

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
 * 
 * \ingroup Operators
 */
template<class TPixel, unsigned int VDimension=2,
  class TAllocator = NeighborhoodAllocator<TPixel> >
class ITK_EXPORT DerivativeOperator
  : public NeighborhoodOperator<TPixel, VDimension, TAllocator>
{

public:
  /** Standard class typedefs. */
  typedef DerivativeOperator Self;
  typedef NeighborhoodOperator<TPixel, VDimension, TAllocator>  Superclass;
  
  typedef typename Superclass::PixelType PixelType;

  /** Constructor. */
  DerivativeOperator() : m_Order(1) {}

  /** Copy constructor. */
  DerivativeOperator(const Self& other)
    : NeighborhoodOperator<TPixel, VDimension, TAllocator>(other)
  { m_Order = other.m_Order;  }
  
  /** Assignment operator */
  Self &operator=(const Self& other)
  {
    Superclass::operator=(other);
    m_Order = other.m_Order;
    return *this;
  }

  /** Sets the order of the derivative. */
  void SetOrder(const unsigned int &order)
  {  m_Order = order;  }

  /** Returns the order of the derivative. */
  unsigned int GetOrder() const { return m_Order; }

  /** Prints some debugging information */
  virtual void PrintSelf(std::ostream &os, Indent i) const  
  { 
    os << i << "DerivativeOperator { this=" << this
       << ", m_Order = " << m_Order << "}" << std::endl;
    Superclass::PrintSelf(os, i.GetNextIndent());
  }
  
protected:
  /** Typedef support for coefficient vector type.  Necessary to
   * work around compiler bug on VC++. */
  typedef typename Superclass::CoefficientVector CoefficientVector;

  /** Calculates operator coefficients. */
  CoefficientVector GenerateCoefficients();

  /** Arranges coefficients spatially in the memory buffer. */
  void Fill(const CoefficientVector &coeff)
  {   Superclass::FillCenteredDirectional(coeff);  }
 
private:
  /** Order of the derivative. */
  unsigned int m_Order;
};

} // namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_DerivativeOperator(_, EXPORT, x, y) namespace itk { \
  _(2(class EXPORT DerivativeOperator< ITK_TEMPLATE_2 x >)) \
  namespace Templates { typedef DerivativeOperator< ITK_TEMPLATE_2 x > \
                                                  DerivativeOperator##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkDerivativeOperator+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkDerivativeOperator.txx"
#endif

#endif


