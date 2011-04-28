/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef __itkDerivativeOperator_h
#define __itkDerivativeOperator_h

#include "itkNeighborhoodOperator.h"

namespace itk
{
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
 * \ingroup ITK-Common
 *
 * \wiki
 * \wikiexample{Operators/DerivativeOperator,Create a derivative kernel}
 * \endwiki
 */
template< class TPixel, unsigned int VDimension = 2,
          class TAllocator = NeighborhoodAllocator< TPixel > >
class ITK_EXPORT DerivativeOperator:
  public NeighborhoodOperator< TPixel, VDimension, TAllocator >
{
public:
  /** Standard class typedefs. */
  typedef DerivativeOperator Self;
  typedef NeighborhoodOperator<
    TPixel, VDimension, TAllocator >           Superclass;

  typedef typename Superclass::PixelType     PixelType;
  typedef typename Superclass::PixelRealType PixelRealType;

  /** Constructor. */
  DerivativeOperator():m_Order(1) {}

  /** Copy constructor. */
  DerivativeOperator(const Self & other):
    NeighborhoodOperator< TPixel, VDimension, TAllocator >(other)
  { m_Order = other.m_Order;  }

  /** Assignment operator */
  Self & operator=(const Self & other)
  {
    Superclass::operator=(other);
    m_Order = other.m_Order;
    return *this;
  }

  /** Sets the order of the derivative. */
  void SetOrder(const unsigned int & order)
  {
    this->m_Order = order;
  }

  /** Returns the order of the derivative. */
  unsigned int GetOrder() const { return m_Order; }

  /** Prints some debugging information */
  virtual void PrintSelf(std::ostream & os, Indent i) const
  {
    os << i << "DerivativeOperator { this=" << this
       << ", m_Order = " << m_Order << "}" << std::endl;
    Superclass::PrintSelf( os, i.GetNextIndent() );
  }

protected:
  /** Typedef support for coefficient vector type.  Necessary to
   * work around compiler bug on VC++. */
  typedef typename Superclass::CoefficientVector CoefficientVector;

  /** Calculates operator coefficients. */
  CoefficientVector GenerateCoefficients();

  /** Arranges coefficients spatially in the memory buffer. */
  void Fill(const CoefficientVector & coeff)
  {   Superclass::FillCenteredDirectional(coeff);  }
private:
  /** Order of the derivative. */
  unsigned int m_Order;
};
} // namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_DerivativeOperator(_, EXPORT, TypeX, TypeY)     \
  namespace itk                                                      \
  {                                                                  \
  _( 2 ( class EXPORT DerivativeOperator< ITK_TEMPLATE_2 TypeX > ) ) \
  namespace Templates                                                \
  {                                                                  \
  typedef DerivativeOperator< ITK_TEMPLATE_2 TypeX >                 \
  DerivativeOperator##TypeY;                                       \
  }                                                                  \
  }

#if ITK_TEMPLATE_EXPLICIT
#include "Templates/itkDerivativeOperator+-.h"
#endif

#if ITK_TEMPLATE_TXX
#include "itkDerivativeOperator.txx"
#endif

#endif
