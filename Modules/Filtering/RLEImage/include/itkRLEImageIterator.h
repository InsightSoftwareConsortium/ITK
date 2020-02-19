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
#ifndef itkRLEImageIterator_h
#define itkRLEImageIterator_h

#include "itkImageIteratorWithIndex.h"
#include "itkRLEImageConstIterator.h"

namespace itk
{
/**
 * \class ImageIterator
 * \brief A multi-dimensional iterator templated over image type.
 *
 * Read-write access. Specialized for RLEImage.
 * \ingroup RLEImage
 * \ingroup ITKCommon
 */

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
class ImageIterator<RLEImage<TPixel, VImageDimension, CounterType>>
  : public ImageConstIterator<RLEImage<TPixel, VImageDimension, CounterType>>
{
public:
  /** Standard class type alias. */
  using Self = ImageIterator;

  /** Dimension of the image the iterator walks.  This constant is needed so
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  static constexpr unsigned int ImageIteratorDimension = VImageDimension;

  /** Define the superclass */
  using Superclass = ImageConstIterator<RLEImage<TPixel, VImageDimension, CounterType>>;

  /** Inherit types from the superclass */
  using IndexType = typename Superclass::IndexType;
  using SizeType = typename Superclass::SizeType;
  using OffsetType = typename Superclass::OffsetType;
  using RegionType = typename Superclass::RegionType;
  using ImageType = typename Superclass::ImageType;
  using InternalPixelType = typename Superclass::InternalPixelType;
  using PixelType = typename Superclass::PixelType;

  /** Default Constructor. Need to provide a default constructor since we
   * provide a copy constructor. */
  ImageIterator() = default;
  /** Default Destructor */
  ~ImageIterator() override = default;
  /** Copy Constructor. The copy constructor is provided to make sure the
   * handle to the image is properly reference counted. */
  ImageIterator(const Self & it)
    : ImageConstIterator<ImageType>(it)
  {}

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageIterator(ImageType * ptr, const RegionType & region)
    : ImageConstIterator<ImageType>(ptr, region)
  {}

  /** operator= is provided to make sure the handle to the image is properly
   * reference counted. */
  Self &
  operator=(const Self & it)
  {
    ImageConstIterator<ImageType>::operator=(it);
    return *this;
  }

  /** Set the pixel value.
   * Changing the RLE structure invalidates all other iterators (except this one). */
  void
  Set(const PixelType & value) const
  {
    const_cast<ImageType *>(this->m_Image.GetPointer())
      ->SetPixel(*const_cast<typename ImageType::RLLine *>(this->m_RunLengthLine),
                 this->m_SegmentRemainder,
                 this->m_RealIndex,
                 value);
  }

  ///** Return a reference to the pixel
  // * Setting this value would change value of the whole run-length segment.
  // * If we wanted to safely enable it,
  // * we would isolate this pixel into its own segment. */
  // PixelType & Value()
  // {
  //    return m_Buffer[m_Index[2]][m_Index[1]][m_RealIndex].second;
  // }

  /** Get the image that this iterator walks. */
  ImageType *
  GetImage() const
  {
    // const_cast is needed here because m_Image is declared as a const pointer
    // in the base class which is the ConstIterator.
    return const_cast<ImageType *>(this->m_Image.GetPointer());
  }

protected:
  /** This constructor is declared protected in order to enforce
    const-correctness */
  ImageIterator(const ImageConstIterator<ImageType> & it)
    : ImageConstIterator<ImageType>(it)
  {}
  Self &
  operator=(const ImageConstIterator<ImageType> & it)
  {
    ImageConstIterator<ImageType>::operator=(it);
    return *this;
  }
};

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
class ImageIteratorWithIndex<RLEImage<TPixel, VImageDimension, CounterType>>
  : public ImageConstIteratorWithIndex<RLEImage<TPixel, VImageDimension, CounterType>>
{
public:
  using ImageType = RLEImage<TPixel, VImageDimension, CounterType>;

  using RegionType = typename itk::ImageConstIterator<RLEImage<TPixel, VImageDimension, CounterType>>::RegionType;

  /** Default Constructor. Need to provide a default constructor since we
   * provide a copy constructor. */
  ImageIteratorWithIndex()
    : ImageConstIteratorWithIndex<ImageType>()
  {}

  /** Copy Constructor. The copy constructor is provided to make sure the
   * handle to the image is properly reference counted. */
  ImageIteratorWithIndex(const ImageIteratorWithIndex & it) { ImageIterator<ImageType>::operator=(it); }

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageIteratorWithIndex(const ImageType * ptr, const RegionType & region)
    : ImageConstIteratorWithIndex<ImageType>(ptr, region)
  {}

  /** Set the pixel value.
   * Changing the RLE structure invalidates all other iterators (except this one). */
  void
  Set(const TPixel & value) const
  {
    const_cast<ImageType *>(this->m_Image.GetPointer())
      ->SetPixel(*const_cast<typename ImageType::RLLine *>(this->m_RunLengthLine),
                 this->m_SegmentRemainder,
                 this->m_RealIndex,
                 value);
  }

  /** Get the image that this iterator walks. */
  ImageType *
  GetImage() const
  {
    // const_cast is needed here because m_Image is declared as a const pointer
    // in the base class which is the ConstIterator.
    return const_cast<ImageType *>(this->m_Image.GetPointer());
  }
}; // no additional implementation required
} // end namespace itk

#endif // itkRLEImageIterator_h
