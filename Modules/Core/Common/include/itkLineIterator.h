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
#ifndef itkLineIterator_h
#define itkLineIterator_h

#include "itkLineConstIterator.h"

namespace itk
{
/**
 * \class LineIterator
 * \brief An iterator that walks a Bresenham line through an ND image
 *        with write access to pixels.
 *
 * LineIterator is an iterator that walks a Bresenham line
 * through an image.  The iterator is constructed similar to other
 * image iterators, except instead of specifying a region to
 * traverse, you specify two indices. The interval specified by
 * the two indices is closed.  So, a line iterator specified with
 * the same start and end index will visit exactly one pixel.
 *
   \code
   LineConstIterator<ImageType> it(image, I1, I2);
   while (!it.IsAtEnd())
   {
      // visits at least 1 pixel
   }
   \endcode
 *
 * \author Benjamin King, Experimentelle Radiologie, Medizinische
 * Hochschule Hannover.
 *
 * \sa LineConstIterator
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/IterateLineThroughImage,Iterate Line Through Image}
 * \endsphinx
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT LineIterator : public LineIteratorBase<TImage, /*VIsConst=*/false>
{
public:
  using Superclass = LineIteratorBase<TImage, /*VIsConst=*/false>;
  using Superclass::Superclass;
};

template <typename TImage>
LineIterator(SmartPointer<TImage>, const typename TImage::RegionType &) -> LineIterator<std::remove_const_t<TImage>>;

template <typename TImage>
LineIterator(TImage *, const typename TImage::RegionType &) -> LineIterator<TImage>;

template <typename TImage>
LineIterator(const TImage *, const typename TImage::RegionType &) -> LineIterator<TImage>;

} // end namespace itk

#endif
