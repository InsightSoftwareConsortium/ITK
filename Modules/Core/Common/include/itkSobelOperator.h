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
#ifndef itkSobelOperator_h
#define itkSobelOperator_h

#include "itkNeighborhoodOperator.h"

namespace itk
{
/**
 * \class SobelOperator
 *
 * \brief A NeighborhoodOperator for performing a directional Sobel
 * edge-detection operation at a pixel location.
 *
 * SobelOperator is a directional NeighborhoodOperator that should be
 * applied a NeighborhoodIterator using the NeighborhoodInnerProduct
 * method. To create the operator:
 *
 * 1) Set the direction by calling  \code SetDirection \endcode
 * 2) call
 * \code
 * itk::Size<2> radius;
 * radius.Fill(1);
 * sobelOperator.CreateToRadius(radius);
 * \endcode
 * 3) You may optionally scale the coefficients of this operator using the
 * \code ScaleCoefficients \endcode method.  This is useful if you
 * want to take the spacing of the image into account when computing
 * the edge strength.  Apply the scaling only after calling to
 * \code CreateToRadius \endcode.
 *
 * The Sobel Operator in vertical direction for 2 dimensions is
 * \verbatim
 *             -1  -2  -1
 *             0    0   0
 *             1    2   1
 *
 * \endverbatim
 * The Sobel Operator in horizonal direction is for 2 dimensions is
 * \verbatim
 *             -1   0   1
 *             -2   0   2
 *             -1   0   1
 * \endverbatim
 *
 * The current implementation of the Sobel operator is for 2 and 3 dimensions only.
 * The ND version is planned for future releases.
 *
 * The extension to 3D is from the publication
 *  "Irwin Sobel. An Isotropic 3x3x3 Volume Gradient Operator.
 * Technical report, Hewlett-Packard Laboratories, April 1995."
 *
 * The Sobel operator in 3D has the kernel
 *
 * \verbatim
 * -1 -3 -1   0 0 0  1 3 1
 * -3 -6 -3   0 0 0  3 6 3
 * -1 -3 -1   0 0 0  1 3 1
 *
 *    x-1       x     x+1
 * \endverbatim
 *
 * The \c x kernel is just rotated as required to obtain the kernel in the
 * \c y and \c z directions.
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
 * \wikiexample{Operators/SobelOperator,Create the Sobel kernel}
 * \endwiki
 */
template< typename TPixel, unsigned int VDimension = 2,
          typename TAllocator = NeighborhoodAllocator< TPixel > >
class ITK_TEMPLATE_EXPORT SobelOperator:
  public NeighborhoodOperator< TPixel, VDimension, TAllocator >
{
public:
  /** Standard typedefs */
  typedef SobelOperator                                          Self;
  typedef NeighborhoodOperator< TPixel, VDimension, TAllocator > Superclass;

  itkTypeMacro(SobelOperator, NeighborhoodOperator);

  SobelOperator() {}
  SobelOperator(const Self & other):
    NeighborhoodOperator< TPixel, VDimension, TAllocator >(other)
  {}

  /** Creates the operator with length only in the specified direction.  For
   * the Sobel operator, this
   * The radius of the operator will be 0 except along the axis on which
   * the operator will work.
   * \sa CreateToRadius \sa FillCenteredDirectional \sa SetDirection() \sa GetDirection() */
  virtual void CreateDirectional() ITK_OVERRIDE
  {
    this->CreateToRadius(1);
  }

  /** Creates the operator with a specified radius ("square", same length
   * on each side). The spatial location of the coefficients within the
   * operator is defined by the subclass implementation of the Fill method.
   * \sa CreateDirectional \sa Fill */
  // virtual void CreateToRadius(const unsigned long);
  /**
   * Assignment operator
   */
  Self & operator=(const Self & other)
  {
    Superclass::operator=(other);
    return *this;
  }

  /**
   * Prints some debugging information
   */
  virtual void PrintSelf(std::ostream & os, Indent i) const ITK_OVERRIDE
  {
    os << i << "SobelOperator { this=" << this  << "}" << std::endl;
    Superclass::PrintSelf( os, i.GetNextIndent() );
  }

protected:
  /**
   * Typedef support for coefficient vector type.  Necessary to
   * work around compiler bug on VC++.
   */
  typedef typename Superclass::CoefficientVector CoefficientVector;
  typedef typename Superclass::PixelType         PixelType;

  /**
   * Calculates operator coefficients.
   */
  CoefficientVector GenerateCoefficients() ITK_OVERRIDE;

  /**
   * Arranges coefficients spatially in the memory buffer.
   */
  void Fill(const CoefficientVector & c) ITK_OVERRIDE;
};
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSobelOperator.hxx"
#endif

#endif
