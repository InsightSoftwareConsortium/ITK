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
#ifndef itkImageScanlineIterator_h
#define itkImageScanlineIterator_h

#include "itkImageScanlineConstIterator.h"

namespace itk
{
/** \class ImageScanlineIterator
 * \brief A multi-dimensional iterator templated over image type that walks a
 * region of pixels, scanline by scanline or in the direction of the
 * fastest axis.
 *
 * The itk::ImageScanlineIterator is optimized for iteration speed and is the
 * first choice for pixel-wise operations on an image.
 * This iterator is preferred over the older ImageRegionIterator even when knowledge
 * of the current line state is not desired because of its speed.
 *
 * \sa ImageScanlineConstIterator
 * \sa ImageRegionIterator
 * \ingroup ImageIterators
 * \ingroup ITKCommon
 *
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ImageScanlineIterator : public ImageScanlineIteratorBase<TImage, /*VIsConst=*/false>
{
public:
  using Superclass = ImageScanlineIteratorBase<TImage, /*VIsConst=*/false>;
  using Superclass::Superclass;
};

template <typename TImage>
ImageScanlineIterator(SmartPointer<TImage>, const typename TImage::RegionType &)
  -> ImageScanlineIterator<std::remove_const_t<TImage>>;

template <typename TImage>
ImageScanlineIterator(TImage *, const typename TImage::RegionType &) -> ImageScanlineIterator<TImage>;

template <typename TImage>
ImageScanlineIterator(const TImage *, const typename TImage::RegionType &) -> ImageScanlineIterator<TImage>;

} // end namespace itk

#endif
