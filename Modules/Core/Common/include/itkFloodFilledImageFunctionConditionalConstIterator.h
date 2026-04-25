/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkFloodFilledImageFunctionConditionalConstIterator_h
#define itkFloodFilledImageFunctionConditionalConstIterator_h

#include "itkFloodFilledFunctionConditionalConstIterator.h"

namespace itk
{
/**
 * \class FloodFilledImageFunctionConditionalConstIterator
 * \brief Iterates over a flood-filled image function with read-only
 *        access to pixels.
 *
 * \ingroup ImageIterators
 *
 * \ingroup ITKCommon
 */
template <typename TImage, typename TFunction, bool VIsConst>
class ITK_TEMPLATE_EXPORT FloodFilledImageFunctionConditionalIteratorBase
  : public FloodFilledFunctionConditionalIteratorBase<TImage, TFunction, VIsConst>
{
public:
  /** Standard class type aliases. */
  using Self = FloodFilledImageFunctionConditionalIteratorBase;
  using Superclass = FloodFilledFunctionConditionalIteratorBase<TImage, TFunction, VIsConst>;

  /** Type of function */
  using typename Superclass::FunctionType;

  /** Type of vector used to store location info in the spatial function */
  using typename Superclass::FunctionInputType;

  /** Index type alias support. */
  using typename Superclass::IndexType;

  /** Index ContainerType. */
  using typename Superclass::SeedsContainerType;

  /** Size type alias support. */
  using typename Superclass::SizeType;

  /** Region type alias support */
  using typename Superclass::RegionType;

  /** Image type alias support. */
  using typename Superclass::ImageType;

  /** Internal Pixel Type */
  using typename Superclass::InternalPixelType;

  /** External Pixel Type */
  using typename Superclass::PixelType;

  using typename Superclass::ImagePointer;

  /** Dimension of the image the iterator walks.  This constant is needed so
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  static constexpr unsigned int NDimensions = Superclass::NDimensions;

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. This version of the constructor uses
   * an explicit seed pixel for the flood fill, the "startIndex" */
  FloodFilledImageFunctionConditionalIteratorBase(ImagePointer imagePtr, FunctionType * fnPtr, IndexType startIndex)
    : Superclass(imagePtr, fnPtr, startIndex)
  {}

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. This version of the constructor uses
   * an explicit list of seed pixels for the flood fill, the "startIndex" */
  FloodFilledImageFunctionConditionalIteratorBase(ImagePointer             imagePtr,
                                                  FunctionType *           fnPtr,
                                                  std::vector<IndexType> & startIndex)
    : Superclass(imagePtr, fnPtr, startIndex)
  {}

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. This version of the constructor
   * should be used when the seed pixel is unknown. */
  FloodFilledImageFunctionConditionalIteratorBase(ImagePointer imagePtr, FunctionType * fnPtr)
    : Superclass(imagePtr, fnPtr)
  {}
  /** Default Destructor. */
  ~FloodFilledImageFunctionConditionalIteratorBase() override = default;

  /** Set the pixel value. SFINAE-gated on !VIsConst. */
  template <bool VCopy = VIsConst, std::enable_if_t<!VCopy, int> = 0>
  void
  Set(const PixelType & value)
  {
    this->m_Image->GetPixel(this->m_IndexStack.front()) = value;
  }

  /** Compute whether the index of interest should be included in the flood */
  bool
  IsPixelIncluded(const IndexType & index) const override;
};

template <typename TImage, typename TFunction>
class ITK_TEMPLATE_EXPORT FloodFilledImageFunctionConditionalConstIterator
  : public FloodFilledImageFunctionConditionalIteratorBase<TImage, TFunction, /*VIsConst=*/true>
{
public:
  using Superclass = FloodFilledImageFunctionConditionalIteratorBase<TImage, TFunction, /*VIsConst=*/true>;
  using Superclass::Superclass;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkFloodFilledImageFunctionConditionalConstIterator.hxx"
#endif

#endif
