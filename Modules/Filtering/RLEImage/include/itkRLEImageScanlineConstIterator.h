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
#ifndef itkRLEImageScanlineConstIterator_h
#define itkRLEImageScanlineConstIterator_h

#include "itkImageScanlineIterator.h"
#include "itkRLEImageRegionConstIterator.h"

namespace itk
{
/** \class ImageScanlineConstIterator
 *  \brief A multi-dimensional iterator templated over image type that
 *  walks a region of pixels, scanline by scanline or in the direction
 *  of the fastest axis. Specialized for RLEImage.
 *  \ingroup RLEImage
 *  \ingroup ITKCommon
 */
template <typename TPixel, unsigned int VImageDimension, typename CounterType>
class ImageScanlineConstIterator<RLEImage<TPixel, VImageDimension, CounterType>>
  : public ImageRegionConstIterator<RLEImage<TPixel, VImageDimension, CounterType>>
{
public:
  /** Standard class type alias. */
  using Self = ImageScanlineConstIterator;
  using Superclass = ImageRegionConstIterator<RLEImage<TPixel, VImageDimension, CounterType>>;

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
  itkTypeMacro(ImageScanlineConstIterator, ImageRegionConstIterator);

  /** Default constructor. Needed since we provide a cast constructor. */
  ImageScanlineConstIterator()
    : ImageRegionConstIterator<ImageType>()
  {}

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageScanlineConstIterator(const ImageType * ptr, const RegionType & region)
    : ImageRegionConstIterator<ImageType>(ptr, region)
  {}

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageScanlineConstIterator. Many routines return an ImageIterator, but for a
   * particular task, you may want an ImageScanlineConstIterator.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageScanlineConstIterator. */
  ImageScanlineConstIterator(const ImageIterator<ImageType> & it)
    : ImageRegionConstIterator<ImageType>(it)
  {}

  /** Constructor that can be used to cast from an ImageConstIterator to an
   * ImageScanlineConstIterator. Many routines return an ImageIterator, but for a
   * particular task, you may want an ImageScanlineConstIterator.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageScanlineConstIterator. */
  ImageScanlineConstIterator(const ImageConstIterator<ImageType> & it)
  {
    ImageRegionConstIterator<ImageType>::operator=(it);
  }

  /** Go to the beginning pixel of the current line. */
  void
  GoToBeginOfLine()
  {
    this->m_Index0 = this->m_BeginIndex0;
    this->m_RealIndex = 0;
    this->m_SegmentRemainder = (*this->m_RunLengthLine)[this->m_RealIndex].first;
  }

  /** Go to the past end pixel of the current line. */
  void
  GoToEndOfLine()
  {
    this->m_Index0 = this->m_EndIndex0;
    this->m_RealIndex = this->m_RunLengthLine->size() - 1;
    this->m_SegmentRemainder = 0;
  }

  /** Test if the index is at the end of line. */
  inline bool
  IsAtEndOfLine()
  {
    return this->m_Index0 == this->m_EndIndex0;
  }

  /** Go to the next line. */
  inline void
  NextLine()
  {
    ++(this->m_BI);
    if (!this->m_BI.IsAtEnd())
    {
      this->SetIndexInternal(this->m_BeginIndex0);
    }
    else
    {
      this->m_Index0 = this->m_BeginIndex0; // make this iterator at end too
    }
  }

  /** Increment (prefix) along the scanline.
   *
   * If the iterator is at the end of the scanline ( one past the last
   * valid element in the row ), then the results are undefined. Which
   * means is may assert in debug mode or result in an undefined
   * iterator which may have unknown consequences if used.
   */
  Self &
  operator++()
  {
    itkAssertInDebugAndIgnoreInReleaseMacro(!this->IsAtEndOfLine());
    this->m_Index0++;
    this->m_SegmentRemainder--;
    if (this->m_SegmentRemainder > 0)
    {
      return *this;
    }

    if (this->IsAtEndOfLine())
    {
      return *this;
    }
    this->m_RealIndex++;
    this->m_SegmentRemainder = (*this->m_RunLengthLine)[this->m_RealIndex].first;
    return *this;
  } // ++

  /** Decrement (prefix) along the scanline.
   *
   */
  Self &
  operator--()
  {
    this->m_Index0--;
    this->m_SegmentRemainder++;
    if (this->m_SegmentRemainder <= (*this->m_RunLengthLine)[this->m_RealIndex].first)
    {
      return *this;
    }

    this->m_RealIndex--;
    this->m_SegmentRemainder = 1;
    return *this;
  }
};
} // end namespace itk

#endif // itkRLEImageScanlineConstIterator_h
