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
#ifndef itkShapedFloodFilledImageFunctionConditionalIterator_h
#define itkShapedFloodFilledImageFunctionConditionalIterator_h

#include "itkShapedFloodFilledImageFunctionConditionalConstIterator.h"

namespace itk
{
/**
 * \class ShapedFloodFilledImageFunctionConditionalIterator
 * \brief Iterates over a flood-filled image function with write access
 *        to pixels.
 *
 * Contributed as a paper to the Insight Journal:
 * https://www.insight-journal.org/browse/publication/204
 *
 * \ingroup ImageIterators
 *
 * \ingroup ITKCommon
 */
template <typename TImage, typename TFunction>
class ShapedFloodFilledImageFunctionConditionalIterator
  : public ShapedFloodFilledImageFunctionConditionalConstIterator<TImage, TFunction>
{
public:
  /** Standard class type aliases. */
  using Self = ShapedFloodFilledImageFunctionConditionalIterator;
  using Superclass = ShapedFloodFilledImageFunctionConditionalConstIterator<TImage, TFunction>;

  /** Type of function */
  using FunctionType = typename Superclass::FunctionType;

  /** Type of vector used to store location info in the spatial function */
  using FunctionInputType = typename Superclass::FunctionInputType;

  /** Index type alias support */
  using IndexType = typename Superclass::IndexType;

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

  /** Dimension of the image the iterator walks.  This constant is needed so
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  static constexpr unsigned int NDimensions = Superclass::NDimensions;

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. This version of the constructor uses
   * an explicit seed pixel for the flood fill, the "startIndex" */
  ShapedFloodFilledImageFunctionConditionalIterator(ImageType * imagePtr, FunctionType * fnPtr, IndexType startIndex)
    : Superclass(imagePtr, fnPtr, startIndex)
  {}

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. This version of the constructor uses
   * an explicit list of seed pixels for the flood fill, the "startIndex" */
  ShapedFloodFilledImageFunctionConditionalIterator(ImageType *              imagePtr,
                                                    FunctionType *           fnPtr,
                                                    std::vector<IndexType> & startIndex)
    : Superclass(imagePtr, fnPtr, startIndex)
  {}

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. This version of the constructor
   * should be used when the seed pixel is unknown. */
  ShapedFloodFilledImageFunctionConditionalIterator(ImageType * imagePtr, FunctionType * fnPtr)
    : Superclass(imagePtr, fnPtr)
  {}

  /** Get the pixel value */
  const PixelType
  Get() const override
  {
    return const_cast<ImageType *>(this->m_Image.GetPointer())->GetPixel(this->m_IndexStack.front());
  }

  /** Set the pixel value */
  void
  Set(const PixelType & value)
  {
    const_cast<ImageType *>(this->m_Image.GetPointer())->GetPixel(this->m_IndexStack.front()) = value;
  }

  /** Default Destructor. */
  ~ShapedFloodFilledImageFunctionConditionalIterator() override = default;
};
} // end namespace itk

#endif
