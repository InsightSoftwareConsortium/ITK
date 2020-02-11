/*=========================================================================
 *
 *  Copyright NumFOCUS
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
   \code
         using DerivativeOperatorType = itk::DerivativeOperator<float, 2>;
         DerivativeOperatorType derivativeOperator;
         derivativeOperator.SetDirection(0); // X dimension
         itk::Size<2> radius;
         radius.Fill(1); // A radius of 1 in both dimensions is a 3x3 operator
         derivativeOperator.CreateToRadius(radius);
   \endcode
 * and creates a kernel that looks like:
   \code
         0        0 0
         0.5  0   -0.5
         0    0   0
   \endcode
 *
 * \note DerivativeOperator does not have any user-declared "special member function",
 * following the C++ Rule of Zero: the compiler will generate them if necessary.
 *
 * \sa NeighborhoodOperator
 * \sa Neighborhood
 * \sa ForwardDifferenceOperator
 * \sa BackwardDifferenceOperator
 *
 * \ingroup Operators
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/CreateDerivativeKernel,Create Derivative Kernel}
 * \endsphinx
 */
template <typename TPixel, unsigned int VDimension = 2, typename TAllocator = NeighborhoodAllocator<TPixel>>
class ITK_TEMPLATE_EXPORT DerivativeOperator : public NeighborhoodOperator<TPixel, VDimension, TAllocator>
{
public:
  /** Standard class type aliases. */
  using Self = DerivativeOperator;
  using Superclass = NeighborhoodOperator<TPixel, VDimension, TAllocator>;

  using PixelType = typename Superclass::PixelType;
  using PixelRealType = typename Superclass::PixelRealType;

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

  /** Prints some debugging information */
  void
  PrintSelf(std::ostream & os, Indent i) const override
  {
    os << i << "DerivativeOperator { this=" << this << ", m_Order = " << m_Order << "}" << std::endl;
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
  /** Order of the derivative. */
  unsigned int m_Order{ 1 };
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkDerivativeOperator.hxx"
#endif

#endif
