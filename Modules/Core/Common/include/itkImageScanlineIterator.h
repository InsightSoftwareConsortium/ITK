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
class ITK_TEMPLATE_EXPORT ImageScanlineIterator : public ImageScanlineConstIterator<TImage>
{
public:
  /** Standard class type aliases. */
  using Self = ImageScanlineIterator;
  using Superclass = ImageScanlineConstIterator<TImage>;

  /** Types inherited from the Superclass */
  using typename Superclass::IndexType;
  using typename Superclass::SizeType;
  using typename Superclass::OffsetType;
  using typename Superclass::RegionType;
  using typename Superclass::ImageType;
  using typename Superclass::PixelContainer;
  using typename Superclass::PixelContainerPointer;
  using typename Superclass::InternalPixelType;
  using typename Superclass::PixelType;
  using typename Superclass::AccessorType;

  /** Default constructor. Needed since we provide a cast constructor. */
  ImageScanlineIterator() = default;

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageScanlineIterator(TImage * ptr, const RegionType & region);

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageScanlineIterator. Many routines return an ImageIterator but for a
   * particular task, you may want an ImageScanlineIterator.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageScanlineIterator. */
  ImageScanlineIterator(const ImageIterator<TImage> & it);

  /** Set the pixel value */
  void
  Set(const PixelType & value) const
  {
    this->m_PixelAccessorFunctor.Set(*(const_cast<InternalPixelType *>(this->m_Buffer + this->m_Offset)), value);
  }

  /** Return a reference to the pixel
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  PixelType &
  Value()
  {
    return *(const_cast<InternalPixelType *>(this->m_Buffer + this->m_Offset));
  }

protected:
  /** the construction from a const iterator is declared protected
      in order to enforce const correctness. */
  ImageScanlineIterator(const ImageScanlineConstIterator<TImage> & it);
  Self &
  operator=(const ImageScanlineConstIterator<TImage> & it);
};

// Deduction guide for class template argument deduction (CTAD).
template <typename TImage>
ImageScanlineIterator(SmartPointer<TImage>, const typename TImage::RegionType &)->ImageScanlineIterator<TImage>;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageScanlineIterator.hxx"
#endif

#endif
