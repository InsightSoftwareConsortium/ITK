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
#ifndef itkHigherOrderAccurateDerivativeOperator_h
#define itkHigherOrderAccurateDerivativeOperator_h

#include "itkNeighborhoodOperator.h"

namespace itk
{

/**
 * \class HigherOrderAccurateDerivativeOperator
 * \brief A NeighborhoodOperator for calculating an n-th order accurate derivative
 * at a pixel.
 *
 * \brief Calculate the image derivative from a higher order accurate
 * central-difference derivative kernel.
 *
 * Based on the work here:
 *
 * Khan, IR and Ohba, Ryoji.  "Closed-form expressions for the finite difference
 * approximations of first and higher derivatives based on Taylor series."
 * Journal of Computational and Applied Mathematics.  vol 107.  p. 179-193.
 * 1999.
 *
 * Khan, IR and Ohba, Ryoji.  "Taylor series based finite difference
 * approximations of higher-degree derivatives."  Journal of Computational and
 * Applied Mathematics.  vol 154.  p. 115-124.  2003.
 *
 * To specify the order of accuracy, use SetOrderOfAccuracy().  The
 * approximation will be accurate to two times the OrderOfAccuracy in terms of
 * Taylor series terms.
 *
 * @todo: implement support for higher order derivatives.
 *
 * \sa DerivativeOperator
 * \sa NeighborhoodOperator
 * \sa Neighborhood
 * \sa ForwardDifferenceOperator
 * \sa BackwardDifferenceOperator
 *
 * \ingroup Operators
 * \ingroup HigherOrderAccurateGradient
 */
template <typename TPixel, unsigned int VDimension = 2, typename TAllocator = NeighborhoodAllocator<TPixel>>
class HigherOrderAccurateDerivativeOperator : public NeighborhoodOperator<TPixel, VDimension, TAllocator>
{
public:
  /** Standard class type alias. */
  using Self = HigherOrderAccurateDerivativeOperator;
  using Superclass = NeighborhoodOperator<TPixel, VDimension, TAllocator>;

  using PixelType = typename Superclass::PixelType;
  using PixelRealType = typename Superclass::PixelRealType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(HigherOrderAccurateDerivativeOperator, NeighborhoodOperator);

  /** Constructor. */
  HigherOrderAccurateDerivativeOperator()
    : m_Order(1)
    , m_OrderOfAccuracy(2)
  {}

  /** Copy constructor. */
  HigherOrderAccurateDerivativeOperator(const Self & other)
    : NeighborhoodOperator<TPixel, VDimension, TAllocator>(other)
  {
    m_Order = other.m_Order;
  }

  /** Assignment operator */
  Self &
  operator=(const Self & other)
  {
    Superclass::operator=(other);
    m_Order = other.m_Order;
    return *this;
  }

  /** Sets the order of the derivative. */
  void
  SetOrder(const unsigned int & order)
  {
    this->m_Order = order;
  }

  /** Returns the order of the derivative. */
  unsigned int
  GetOrder() const
  {
    return m_Order;
  }

  /** Sets the order of accuracy of the derivative. The derivative estimate will
   * be accurate out to two times the given order in terms of Taylor Series terms.  The
   * radius of the neighborhood operator is also equal to the given order.  */
  void
  SetOrderOfAccuracy(const unsigned int & order)
  {
    this->m_OrderOfAccuracy = order;
  }

  unsigned int
  GetOrderOfAccuracy() const
  {
    return m_OrderOfAccuracy;
  }

  /** Prints some debugging information */
  void
  PrintSelf(std::ostream & os, Indent i) const override
  {
    os << i << "HigherOrderAccurateDerivativeOperator { this=" << this << ", m_Order = " << m_Order
       << ", m_OrderOfAccuracy = " << m_OrderOfAccuracy << "}" << std::endl;
    Superclass::PrintSelf(os, i.GetNextIndent());
  }

protected:
  /** Typedef support for coefficient vector type.  Necessary to
   * work around compiler bug on VC++. */
  using CoefficientVector = typename Superclass::CoefficientVector;

  /** Calculates operator coefficients. */
  CoefficientVector
  GenerateCoefficients() override;

  /** Arranges coefficients spatially in the memory buffer. */
  void
  Fill(const CoefficientVector & coeff) override
  {
    Superclass::FillCenteredDirectional(coeff);
  }

private:
  CoefficientVector
  GenerateFirstOrderCoefficients();

  /** Order of the derivative. */
  unsigned int m_Order;

  /** Order of accuracy. */
  unsigned int m_OrderOfAccuracy;
};

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkHigherOrderAccurateDerivativeOperator.hxx"
#endif

#endif
