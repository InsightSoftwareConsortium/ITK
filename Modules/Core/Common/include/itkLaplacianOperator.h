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
#ifndef itkLaplacianOperator_h
#define itkLaplacianOperator_h

#include "itkNeighborhoodOperator.h"

namespace itk
{
/**
 * \class LaplacianOperator
 * \brief A NeighborhoodOperator for use in calculating the Laplacian at a pixel.
 *
 *  A NeighborhoodOperator for use in calculating the Laplacian at a pixel.
 *  The LaplacianOperator's coefficients are a tightest-fitting convolution
 *  kernel.
 *
 *  For example, the simplest Laplacian Operator for 2D has the form:
    \code
                0   1   0
                1  -4   1
                0   1   0
    \endcode
 *
 *  \par
 *  The LaplacianOperator is a non-directional NeighborhoodOperator that
 *  should be applied to a Neighborhood or NeighborhoodIterator using an inner
 *  product method (itkNeighborhoodInnerProduct).  To initialize the operator, you
 *  need call CreateOperator() before using it.
 *
 *  \par
 *  By default the operator will be created for an isotropic image, but you can
 *  modify the operator to handle different pixel spacings by calling
 *  SetDerivativeScalings.  The argument to SetDerivativeScalings is an array
 *  of doubles that is of length VDimension (the dimensionality of the image).
 *  Make sure to use 1/pixel_spacing to properly scale derivatives.
 *
 * \note LaplacianOperator does not have any user-declared "special member function"
 * for copy, move, or destruction, following the C++ Rule of Zero: the compiler will
 * generate them if necessary.
 *
 * \sa NeighborhoodOperator
 * \sa Neighborhood
 * \ingroup Operators
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/CreateLaplacianKernel,Create Laplacian Kernel}
 * \endsphinx
 */
template <typename TPixel, unsigned int VDimension = 2, typename TAllocator = NeighborhoodAllocator<TPixel>>
class ITK_TEMPLATE_EXPORT LaplacianOperator : public NeighborhoodOperator<TPixel, VDimension, TAllocator>
{
public:
  /** Standard "Self" type alias support   */
  using Self = LaplacianOperator;

  /** Standard "Superclass" type alias.   */
  using Superclass = NeighborhoodOperator<TPixel, VDimension, TAllocator>;

  using PixelType = typename Superclass::PixelType;
  using SizeType = typename Superclass::SizeType;

  /**  Default constructor  */
  LaplacianOperator()
  {
    for (unsigned i = 0; i < VDimension; ++i)
    {
      m_DerivativeScalings[i] = 1.0;
    }
  }

  /** This function is called to create the operator  */
  void
  CreateOperator();

  /** Prints some debugging information   */
  void
  PrintSelf(std::ostream & os, Indent i) const override
  {
    os << i << "LaplacianOperator { this=" << this << "}" << std::endl;
    Superclass::PrintSelf(os, i.GetNextIndent());
  }

  /** Sets the weights that are applied to the derivative in each axial
   *  direction when the kernel is computed.  These weights are all 1.0 by
   *  default. This method must be called BEFORE CreateOperator */
  void
  SetDerivativeScalings(const double * s);

protected:
  /** Typedef support for coefficient vector type.  Necessary to
   * work around compiler bug on VC++.   */
  using CoefficientVector = typename Superclass::CoefficientVector;

  /** Calculates operator coefficients.   */
  CoefficientVector
  GenerateCoefficients() override;

  /** Arranges coefficients spatially in the memory buffer, default
   * function was NOT used.   */
  void
  Fill(const CoefficientVector &) override;

private:
  /** Weights applied to derivatives in each axial direction */
  double m_DerivativeScalings[VDimension];
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLaplacianOperator.hxx"
#endif

#endif
