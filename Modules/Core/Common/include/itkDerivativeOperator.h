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
#ifndef itkDerivativeOperator_h
#define itkDerivativeOperator_h

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
 * An example operator to compute X derivatives of a 2D image can be
 * created with:
 * \code
 *       typedef itk::DerivativeOperator<float, 2> DerivativeOperatorType;
 *       DerivativeOperatorType derivativeOperator;
 *       derivativeOperator.SetDirection(0); // X dimension
 *       itk::Size<2> radius;
 *       radius.Fill(1); // A radius of 1 in both dimensions is a 3x3 operator
 *       derivativeOperator.CreateToRadius(radius);
 * \endcode
 * and creates a kernel that looks like:
 * \code
 *       0        0 0
 *       0.5  0   -0.5
 *       0    0   0
 * \endcode
 *
 * \sa NeighborhoodOperator
 * \sa Neighborhood
 * \sa ForwardDifferenceOperator
 * \sa BackwardDifferenceOperator
 *
 * \ingroup Operators
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{Operators/DerivativeOperator,Create a derivative kernel}
 * \endwiki
 */
template< typename TPixel, unsigned int VDimension = 2,
          typename TAllocator = NeighborhoodAllocator< TPixel > >
class ITK_TEMPLATE_EXPORT DerivativeOperator:
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
    if(this != &other)
      {
      Superclass::operator=(other);
      m_Order = other.m_Order;
      }
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

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkDerivativeOperator.hxx"
#endif

#endif
