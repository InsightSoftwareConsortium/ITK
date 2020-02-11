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
#ifndef itkNeighborhoodInnerProduct_h
#define itkNeighborhoodInnerProduct_h

#include "itkNeighborhoodIterator.h"
#include "itkConstSliceIterator.h"
#include "itkImageBoundaryCondition.h"

namespace itk
{
/** \class NeighborhoodInnerProduct
 *  \brief Defines the inner product operation between an
 *         Neighborhood and a NeighborhoodOperator.
 *
 * This class defines the inner product operation between an
 * itk::Neighborhood and an itk::NeighborhoodOperator.  The
 * operator() method is overloaded to support various types of
 * neighborhoods as well as inner products with slices of
 * neighborhoods.
 *
 * \tparam TImage         Type of image on which the class operates.
 * \tparam TOperator      The value type of the operator (defaults to
 * the image pixel type).
 * \tparam TComputation   The value type used as the return type of the
 * inner product calculation (defaults to the operator type).
 *
 * \ingroup Operators
 * \ingroup ITKCommon
 */

template <typename TImage, typename TOperator = typename TImage::PixelType, typename TComputation = TOperator>
class ITK_TEMPLATE_EXPORT NeighborhoodInnerProduct
{
public:
  /** Standard type alias */
  using Self = NeighborhoodInnerProduct;

  /** Capture some type alias from the template parameters. */
  using ImagePixelType = typename TImage::PixelType;
  using OperatorPixelType = TOperator;
  using OutputPixelType = TComputation;

  /** Capture some type alias from the template parameters. */
  static constexpr unsigned int ImageDimension = TImage::ImageDimension;

  /** Operator type alias */
  using OperatorType = Neighborhood<OperatorPixelType, Self::ImageDimension>;

  using NeighborhoodType = Neighborhood<ImagePixelType, Self::ImageDimension>;

  static OutputPixelType
  Compute(const ConstNeighborhoodIterator<TImage> & it,
          const OperatorType &                      op,
          const unsigned                            start = 0,
          const unsigned                            stride = 1);

  static OutputPixelType
  Compute(const NeighborhoodType & N, const OperatorType & op, const unsigned start = 0, const unsigned stride = 1);

  /** Reference oeprator. */
  OutputPixelType
  operator()(const std::slice & s, const ConstNeighborhoodIterator<TImage> & it, const OperatorType & op) const
  {
    return Self::Compute(it, op, s.start(), s.stride());
  }

  OutputPixelType
  operator()(const ConstNeighborhoodIterator<TImage> & it, const OperatorType & op) const
  {
    return Self::Compute(it, op);
  }

  OutputPixelType
  operator()(const std::slice & s, const NeighborhoodType & N, const OperatorType & op) const
  {
    return Self::Compute(N, op, s.start(), s.stride());
  }

  OutputPixelType
  operator()(const NeighborhoodType & N, const OperatorType & op) const
  {
    return Self::Compute(N, op);
  }
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkNeighborhoodInnerProduct.hxx"
#endif

/*
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodInnerProduct.hxx"
#endif
*/

#endif
