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
#ifndef itkFloodFilledSpatialFunctionConditionalConstIterator_h
#define itkFloodFilledSpatialFunctionConditionalConstIterator_h

#include "itkFloodFilledFunctionConditionalConstIterator.h"

namespace itk
{
/**
 * \class FloodFilledSpatialFunctionConditionalConstIterator
 * \brief Iterates over a flood-filled spatial function with read-only access
 *        to pixels.
 *
 * \ingroup ImageIterators
 *
 * \ingroup ITKCommon
 */
template <typename TImage, typename TFunction>
class ITK_TEMPLATE_EXPORT FloodFilledSpatialFunctionConditionalConstIterator
  : public FloodFilledFunctionConditionalConstIterator<TImage, TFunction>
{
public:
  /** Standard class type aliases. */
  using Self = FloodFilledSpatialFunctionConditionalConstIterator;
  using Superclass = FloodFilledFunctionConditionalConstIterator<TImage, TFunction>;

  /** Type of function */
  using FunctionType = typename Superclass::FunctionType;

  /** Type of vector used to store location info in the spatial function */
  using FunctionInputType = typename Superclass::FunctionInputType;

  /** Index type alias support */
  using IndexType = typename Superclass::IndexType;

  /** Index ContainerType. */
  using SeedsContainerType = typename Superclass::SeedsContainerType;

  /** Size type alias support */
  using SizeType = typename Superclass::SizeType;

  /** Region type alias support */
  using RegionType = typename Superclass::RegionType;

  /** Image type alias support */
  using ImageType = typename Superclass::ImageType;

  /** Internal Pixel Type */
  using InternalPixelType = typename Superclass::InternalPixelType;

  /** External Pixel Type */
  using PixelType = typename Superclass::PixelType;

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. This version of the constructor uses
   * an explicit seed pixel for the flood fill, the "startIndex" */
  FloodFilledSpatialFunctionConditionalConstIterator(const ImageType * imagePtr,
                                                     FunctionType *    fnPtr,
                                                     IndexType         startIndex);

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. This version of the constructor
   * should be used when the seed pixel is unknown. */
  FloodFilledSpatialFunctionConditionalConstIterator(const ImageType * imagePtr, FunctionType * fnPtr);
  /** Default Destructor. */
  ~FloodFilledSpatialFunctionConditionalConstIterator() override = default;

  /** Compute whether the index of interest should be included in the flood */
  bool
  IsPixelIncluded(const IndexType & index) const override;

  /** Set the inclusion strategy to origin */
  void
  SetOriginInclusionStrategy()
  {
    m_InclusionStrategy = 0;
  }

  /** Set the inclusion strategy to center */
  void
  SetCenterInclusionStrategy()
  {
    m_InclusionStrategy = 1;
  }

  /** Set the inclusion strategy to complete */
  void
  SetCompleteInclusionStrategy()
  {
    m_InclusionStrategy = 2;
  }

  /** Set the inclusion strategy to intersect */
  void
  SetIntersectInclusionStrategy()
  {
    m_InclusionStrategy = 3;
  }

protected: // made protected so other iterators can access
  /** How the pixel (index) is examined in order to decide whether or not
   * it's included. The strategies are:
   * 0) Origin: if the origin of the pixel in physical space is inside the function,
   * then the pixel is inside the function
   * 1) Center: if the center of a pixel, in physical space, is inside the function,
   * then the pixel is inside the function
   * 2) Complete: if all of the corners of the pixel in physical space are inside the function,
   * then the pixel is inside the function
   * 3) Intersect: if any of the corners of the pixel in physical space are inside the function,
   * then the pixel is inside the function */

  unsigned char m_InclusionStrategy;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFloodFilledSpatialFunctionConditionalConstIterator.hxx"
#endif

#endif
