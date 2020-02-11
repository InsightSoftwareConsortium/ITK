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
#ifndef itkBackwardDifferenceOperator_h
#define itkBackwardDifferenceOperator_h

#include "itkNeighborhoodOperator.h"

namespace itk
{
/**
 * \class BackwardDifferenceOperator
 *
 * \brief Operator whose inner product with a neighborhood returns
 * a "half" derivative at the center of the neighborhood.
 *
 * BackwardDifferenceOperator uses backward differences
 * i.e. \f$ F(x) - F(x-1) \f$ to calculate a "half" derivative useful, among
 * other things, in solving differential equations. It is a directional
 * NeighborhoodOperator that should be applied to a Neighborhood using the
 * inner product.
 *
 * \note BackwardDifferenceOperator does not have any user-declared "special member function",
 * following the C++ Rule of Zero: the compiler will generate them if necessary.
 *
 * \ingroup Operators
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/CreateABackwardDifferenceOperator,Create A Backward Difference Operator}
 * \endsphinx
 */
template <typename TPixel, unsigned int TDimension = 2, typename TAllocator = NeighborhoodAllocator<TPixel>>
class ITK_TEMPLATE_EXPORT BackwardDifferenceOperator : public NeighborhoodOperator<TPixel, TDimension, TAllocator>
{
public:
  /** Standard class type aliases. */
  using Self = BackwardDifferenceOperator;
  using Superclass = NeighborhoodOperator<TPixel, TDimension, TAllocator>;

  /** From Superclass */
  using PixelType = typename Superclass::PixelType;

protected:
  /** Necessary to work around a compiler bug in VC++. */
  using CoefficientVector = typename Superclass::CoefficientVector;

  /** Calculates operator coefficients. */
  CoefficientVector
  GenerateCoefficients() override;

  /** Arranges coefficients spatially in the memory buffer. */
  void
  Fill(const CoefficientVector & coeff) override
  {
    this->FillCenteredDirectional(coeff);
  }
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkBackwardDifferenceOperator.hxx"
#endif

#endif
