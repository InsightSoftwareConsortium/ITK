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
#ifndef itkImageIterator_h
#define itkImageIterator_h

#include "itkImageConstIterator.h"

namespace itk
{
/**
 * \class ImageIterator
 * \brief A multi-dimensional iterator templated over image type.
 *
 * Preserved as an alias template over
 * ImageIteratorBase<TImage, /\*VIsConst=*\/false>. Write access
 * (Set(), non-const Value()) is SFINAE-enabled directly on the
 * underlying ImageIteratorBase template when VIsConst==false, so
 * there are no const_cast sites.
 *
 * \ingroup ImageIterators
 * \ingroup ITKCommon
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ImageIterator : public ImageIteratorBase<TImage, /*VIsConst=*/false>
{
public:
  using Superclass = ImageIteratorBase<TImage, /*VIsConst=*/false>;
  using Superclass::Superclass;
};

template <typename TImage>
ImageIterator(SmartPointer<TImage>, const typename TImage::RegionType &) -> ImageIterator<std::remove_const_t<TImage>>;

template <typename TImage>
ImageIterator(TImage *, const typename TImage::RegionType &) -> ImageIterator<TImage>;

template <typename TImage>
ImageIterator(const TImage *, const typename TImage::RegionType &) -> ImageIterator<TImage>;

// Deduction guide for class template argument deduction (CTAD).
// The ImageIteratorBase deduction guide in itkImageConstIterator.h
// infers VIsConst from the constness of TImage in the argument, so
// a non-const SmartPointer<TImage> resolves to ImageIterator<TImage>
// and a SmartPointer<const TImage> resolves to ImageConstIterator<TImage>.

} // end namespace itk

#endif
