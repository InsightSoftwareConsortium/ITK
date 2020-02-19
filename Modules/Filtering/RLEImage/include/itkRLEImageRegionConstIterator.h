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
#ifndef itkRLEImageRegionConstIterator_h
#define itkRLEImageRegionConstIterator_h

#include "itkImageRegionConstIterator.h"
#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageRegionConstIteratorWithOnlyIndex.h"
#include "itkRLEImageConstIterator.h"

class MultiLabelMeshPipeline;

namespace itk
{
/** \class ImageRegionConstIterator
 * \brief A multi-dimensional iterator templated over image type that walks a
 * region of pixels.
 *
 * ImageRegionConstIterator provides read-only access to image data.
 * It is the base class for the read/write access ImageRegionIterator.
 * Specialized for RLEImage.
 *
 *  \ingroup RLEImage
 *  \ingroup ITKCommon
 */
template <typename TPixel, unsigned int VImageDimension, typename CounterType>
class ImageRegionConstIterator<RLEImage<TPixel, VImageDimension, CounterType>>
  : public ImageConstIterator<RLEImage<TPixel, VImageDimension, CounterType>>
{
  friend class ::MultiLabelMeshPipeline;

public:
  /** Standard class type alias. */
  using Self = ImageRegionConstIterator<RLEImage<TPixel, VImageDimension, CounterType>>;
  using Superclass = ImageConstIterator<RLEImage<TPixel, VImageDimension, CounterType>>;

  /** Dimension of the image that the iterator walks.  This constant is needed so
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  static constexpr unsigned int ImageIteratorDimension = VImageDimension;

  /**
   * Index type alias support While these were already typdef'ed in the superclass,
   * they need to be redone here for this subclass to compile properly with gcc.
   */
  /** Types inherited from the Superclass */
  using IndexType = typename Superclass::IndexType;
  using SizeType = typename Superclass::SizeType;
  using OffsetType = typename Superclass::OffsetType;
  using RegionType = typename Superclass::RegionType;
  using ImageType = typename Superclass::ImageType;
  using InternalPixelType = typename Superclass::InternalPixelType;
  using PixelType = typename Superclass::PixelType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageRegionConstIterator, ImageConstIterator);

  /** Default constructor. Needed since we provide a cast constructor. */
  ImageRegionConstIterator()
    : ImageConstIterator<ImageType>()
  {}

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageRegionConstIterator(const ImageType * ptr, const RegionType & region)
    : ImageConstIterator<ImageType>(ptr, region)
  {}

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageRegionConstIterator. Many routines return an ImageIterator, but for a
   * particular task, you may want an ImageRegionConstIterator.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageRegionConstIterator. */
  ImageRegionConstIterator(const ImageIterator<ImageType> & it) { ImageConstIterator<ImageType>::operator=(it); }

  /** Constructor that can be used to cast from an ImageConstIterator to an
   * ImageRegionConstIterator. Many routines return an ImageIterator, but for a
   * particular task, you may want an ImageRegionConstIterator.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageRegionConstIterator. */
  ImageRegionConstIterator(const ImageConstIterator<ImageType> & it) { ImageConstIterator<ImageType>::operator=(it); }

  /** Increment (prefix) the fastest moving dimension of the iterator's index.
   * This operator will constrain the iterator within the region (i.e. the
   * iterator will automatically wrap from the end of the row of the region
   * to the beginning of the next row of the region) up until the iterator
   * tries to moves past the last pixel of the region.  Here, the iterator
   * will be set to be one pixel past the end of the region.
   * \sa operator++(int) */
  Self &
  operator++()
  {
    this->m_Index0++;

    if (this->m_Index0 >= this->m_EndIndex0)
    {
      ++(this->m_BI);
      if (!this->m_BI.IsAtEnd())
      {
        this->SetIndexInternal(this->m_BeginIndex0);
      }
      else
      {
        this->m_Index0 = this->m_BeginIndex0;
      }
      return *this;
    }

    this->m_SegmentRemainder--;
    if (this->m_SegmentRemainder > 0)
    {
      return *this;
    }

    this->m_RealIndex++;
    this->m_SegmentRemainder = (*this->m_RunLengthLine)[this->m_RealIndex].first;
    return *this;
  } // ++

  /** Decrement (prefix) the fastest moving dimension of the iterator's index.
   * This operator will constrain the iterator within the region (i.e. the
   * iterator will automatically wrap from the beginning of the row of the region
   * to the end of the next row of the region) up until the iterator
   * tries to moves past the first pixel of the region.  Here, the iterator
   * will be set to be one pixel past the beginning of the region.
   * \sa operator--(int) */
  Self &
  operator--()
  {
    this->m_Index0--;

    if (this->m_Index0 < this->m_BeginIndex0)
    {
      --(this->m_BI);
      this->SetIndexInternal(this->m_EndIndex0 - 1);
      return *this;
    }

    this->m_SegmentRemainder++;
    if (this->m_SegmentRemainder <= (*this->m_RunLengthLine)[this->m_RealIndex].first)
    {
      return *this;
    }

    this->m_RealIndex--;
    this->m_SegmentRemainder = 1;
    return *this;
  } // --
};

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
class ImageRegionConstIteratorWithIndex<RLEImage<TPixel, VImageDimension, CounterType>>
  : public ImageRegionConstIterator<RLEImage<TPixel, VImageDimension, CounterType>>
{
public:
  using ImageType = RLEImage<TPixel, VImageDimension, CounterType>;

  using RegionType = typename itk::ImageConstIterator<RLEImage<TPixel, VImageDimension, CounterType>>::RegionType;

  /** Default constructor. Needed since we provide a cast constructor. */
  ImageRegionConstIteratorWithIndex()
    : ImageRegionConstIterator<ImageType>()
  {}

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageRegionConstIteratorWithIndex(const ImageType * ptr, const RegionType & region)
    : ImageRegionConstIterator<ImageType>(ptr, region)
  {}

  void
  GoToReverseBegin()
  {
    this->m_BI.GoToEnd(); // after last pixel
    --(this->m_BI);       // go to last valid pixel
    this->m_Index0 = this->m_EndIndex0 - 1;
    this->SetIndexInternal(this->m_Index0); // valid index required
  }

  bool
  IsAtReverseEnd()
  {
    return (this->m_Index0 == this->m_BeginIndex0) && this->m_BI.IsAtBegin();
  }

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageRegionConstIterator. Many routines return an ImageIterator, but for a
   * particular task, you may want an ImageRegionConstIterator.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageRegionConstIterator. */
  ImageRegionConstIteratorWithIndex(const ImageIterator<ImageType> & it)
  {
    ImageRegionConstIterator<ImageType>::operator=(it);
  }
}; // no additional implementation required

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
class ImageRegionConstIteratorWithOnlyIndex<RLEImage<TPixel, VImageDimension, CounterType>>
  : public ImageRegionConstIteratorWithIndex<RLEImage<TPixel, VImageDimension, CounterType>>
{
  // just inherit constructors

public:
  using ImageType = RLEImage<TPixel, VImageDimension, CounterType>;

  using RegionType = typename itk::ImageConstIterator<RLEImage<TPixel, VImageDimension, CounterType>>::RegionType;

  /** Default constructor. Needed since we provide a cast constructor. */
  ImageRegionConstIteratorWithOnlyIndex()
    : ImageRegionConstIterator<ImageType>()
  {}

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageRegionConstIteratorWithOnlyIndex(const ImageType * ptr, const RegionType & region)
    : ImageRegionConstIteratorWithIndex<ImageType>(ptr, region)
  {}

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageRegionConstIterator. Many routines return an ImageIterator, but for a
   * particular task, you may want an ImageRegionConstIterator.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageRegionConstIterator. */
  ImageRegionConstIteratorWithOnlyIndex(const ImageIterator<ImageType> & it)
  {
    ImageRegionConstIterator<ImageType>::operator=(it);
  }
}; // no additional implementation required
} // end namespace itk

#endif // itkRLEImageRegionConstIterator_h
