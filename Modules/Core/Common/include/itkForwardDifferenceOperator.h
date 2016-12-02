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
#ifndef itkForwardDifferenceOperator_h
#define itkForwardDifferenceOperator_h

#include "itkNeighborhoodOperator.h"

namespace itk
{
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
 *
 * \ingroup Operators
 * \ingroup ITKCommon
 *
 * \wiki
 * \wikiexample{Operators/ForwardDifferenceOperator,Create a forward difference kernel}
 * \endwiki
 */
template< typename TPixel, unsigned int VDimension = 2,
          typename TAllocator = NeighborhoodAllocator< TPixel > >
class ITK_TEMPLATE_EXPORT ForwardDifferenceOperator:
  public NeighborhoodOperator< TPixel, VDimension, TAllocator >
{
public:
  /** Standard class typedefs. */
  typedef ForwardDifferenceOperator                              Self;
  typedef NeighborhoodOperator< TPixel, VDimension, TAllocator > Superclass;

  typedef typename Superclass::PixelType PixelType;

  /** Constructor. */
  ForwardDifferenceOperator() {}

  /** Copy constructor */
  ForwardDifferenceOperator(const Self & other):
    NeighborhoodOperator< TPixel, VDimension, TAllocator >(other) {}

  /** Assignment operator */
  Self & operator=(const Self & other)
  {
    Superclass::operator=(other);
    return *this;
  }

protected:
  /** Necessary to work around VC++ compiler bug. */
  typedef typename Superclass::CoefficientVector CoefficientVector;

  /** Calculates operator coefficients. */
  CoefficientVector GenerateCoefficients();

  /** Arranges coefficients spatially in the memory buffer. */
  void Fill(const CoefficientVector & coeff)

  { this->FillCenteredDirectional(coeff); }
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkForwardDifferenceOperator.hxx"
#endif

#endif
