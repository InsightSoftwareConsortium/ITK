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
#ifndef itkRLEImageConstIterator_h
#define itkRLEImageConstIterator_h

#include "itkImage.h"
#include "itkImageConstIterator.h"
#include "itkImageConstIteratorWithIndex.h"
#include "itkImageConstIteratorWithOnlyIndex.h"
#include "itkImageRegionIterator.h"
#include "itkIndex.h"
#include "itkNumericTraits.h"
#include "itkRLEImage.h"

class MultiLabelMeshPipeline;

namespace itk
{
/** \class ImageConstIterator
 *  \brief A multi-dimensional image iterator templated over image type.
 *  Specialized for RLEImage.
 *  \ingroup RLEImage
 *  \ingroup ITKCommon
 */

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
class ImageConstIterator<RLEImage<TPixel, VImageDimension, CounterType>>
{
  friend class ::MultiLabelMeshPipeline;

public:
  /** Standard class type alias. */
  using Self = ImageConstIterator;

  /** Dimension of the image the iterator walks.  This constant is needed so
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  static constexpr unsigned int ImageIteratorDimension = VImageDimension;

  /** Run-time type information (and related methods). */
  itkVirtualGetNameOfClassMacro(ImageConstIterator);

  /** Image type alias support. */
  using ImageType = RLEImage<TPixel, VImageDimension, CounterType>;

  /** Run-Length Line (we iterate along it). */
  using RLLine = typename ImageType::RLLine;

  /** Buffer Type used. */
  using BufferType = typename ImageType::BufferType;

  /** Type for the internal buffer iterator. */
  using BufferIterator = ImageRegionIterator<BufferType>;

  /** Index type alias support. */
  using IndexType = typename ImageType::IndexType;

  /** Index type alias support. */
  using IndexValueType = typename ImageType::IndexValueType;

  /** Size type alias support. */
  using SizeType = typename ImageType::SizeType;

  /** Offset type alias support. */
  using OffsetType = typename ImageType::OffsetType;

  /** Region type alias support. */
  using RegionType = typename ImageType::RegionType;

  /** Internal Pixel Type */
  using InternalPixelType = typename ImageType::InternalPixelType;

  /** External Pixel Type */
  using PixelType = typename ImageType::PixelType;

  /** Default Constructor. Need to provide a default constructor since we
   * provide a copy constructor. */
  ImageConstIterator()
    : m_RunLengthLine(nullptr)
    , m_Buffer(nullptr)
  {
    m_Image = nullptr;
    m_Index0 = 0;
    m_BeginIndex0 = 0;
    m_EndIndex0 = 0;
    m_RealIndex = 0;
    m_SegmentRemainder = 0;
  }

  /** Default Destructor. */
  virtual ~ImageConstIterator() = default;
  /** Copy Constructor. The copy constructor is provided to make sure the
   * handle to the image is properly reference counted. */
  ImageConstIterator(const Self & it)
    : m_Buffer(const_cast<ImageType *>(it.GetImage())->GetBuffer())
  {
    m_RunLengthLine = it.m_RunLengthLine;
    m_Image = it.m_Image; // copy the smart pointer
    m_Index0 = it.m_Index0;
    this->m_BI = it.m_BI;

    m_RealIndex = it.m_RealIndex;
    m_SegmentRemainder = it.m_SegmentRemainder;
    m_BeginIndex0 = it.m_BeginIndex0;
    m_EndIndex0 = it.m_EndIndex0;
  }

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageConstIterator(const ImageType * ptr, const RegionType & region)
    : m_Buffer(const_cast<ImageType *>(ptr)->GetBuffer())
  {
    m_Image = ptr;
    SetRegion(region);
  }

  /** operator= is provided to make sure the handle to the image is properly
   * reference counted. */
  Self &
  operator=(const Self & it)
  {
    if (this != &it)
    {
      m_Buffer = it.m_Buffer;
      m_RunLengthLine = it.m_RunLengthLine;
      m_Image = it.m_Image; // copy the smart pointer
      m_Index0 = it.m_Index0;
      m_BI = it.m_BI;

      m_RealIndex = it.m_RealIndex;
      m_SegmentRemainder = it.m_SegmentRemainder;
      m_BeginIndex0 = it.m_BeginIndex0;
      m_EndIndex0 = it.m_EndIndex0;
    }
    return *this;
  }

  /** Set the region of the image to iterate over. */
  virtual void
  SetRegion(const RegionType & region)
  {
    // m_Region = region;

    if (region.GetNumberOfPixels() > 0) // If region is non-empty
    {
      const RegionType & bufferedRegion = m_Image->GetBufferedRegion();
      itkAssertOrThrowMacro((bufferedRegion.IsInside(region)),
                            "Region " << region << " is outside of buffered region " << bufferedRegion);
    }

    m_BI = BufferIterator(m_Buffer, region.Slice(0));
    m_Index0 = region.GetIndex(0);
    m_BeginIndex0 = m_Index0 - m_Image->GetBufferedRegion().GetIndex(0);
    m_EndIndex0 = m_BeginIndex0 + region.GetSize(0);
    SetIndexInternal(m_BeginIndex0); // sets m_RealIndex and m_SegmentRemainder
  }

  /** Get the dimension (size) of the index. */
  static unsigned int
  GetImageIteratorDimension()
  {
    return VImageDimension;
  }

  /** Comparison operator. Two iterators are the same if they "point to" the
   * same memory location */
  bool
  operator!=(const Self & it) const
  {
    return m_BI != it.m_BI || m_Index0 + m_BeginIndex0 != it.m_Index0 + it.m_BeginIndex0;
  }

  /** Comparison operator. Two iterators are the same if they "point to" the
   * same memory location */
  bool
  operator==(const Self & it) const
  {
    return m_BI == it.m_BI && m_Index0 + m_BeginIndex0 == it.m_Index0 + it.m_BeginIndex0;
  }

  /** Comparison operator. An iterator is "less than" another if it "points to"
   * a lower memory location. */
  bool
  operator<=(const Self & it) const
  {
    if (m_BI < it.m_BI)
    {
      return true;
    }
    else if (m_BI > it.m_BI)
    {
      return false;
    }
    return m_Index0 + m_BeginIndex0 <= it.m_Index0 + it.m_BeginIndex0;
  }

  /** Comparison operator. An iterator is "less than" another if it "points to"
   * a lower memory location. */
  bool
  operator<(const Self & it) const
  {
    if (m_BI < it.m_BI)
    {
      return true;
    }
    else if (m_BI > it.m_BI)
    {
      return false;
    }
    return m_Index0 + m_BeginIndex0 < it.m_Index0 + it.m_BeginIndex0;
  }

  /** Comparison operator. An iterator is "greater than" another if it
   * "points to" a higher location. */
  bool
  operator>=(const Self & it) const
  {
    if (m_BI > it.m_BI)
    {
      return true;
    }
    else if (m_BI < it.m_BI)
    {
      return false;
    }
    return m_Index0 + m_BeginIndex0 >= it.m_Index0 + it.m_BeginIndex0;
  }

  /** Comparison operator. An iterator is "greater than" another if it
   * "points to" a higher location. */
  bool
  operator>(const Self & it) const
  {
    if (m_BI > it.m_BI)
    {
      return true;
    }
    else if (m_BI < it.m_BI)
    {
      return false;
    }
    return m_Index0 + m_BeginIndex0 > it.m_Index0 + it.m_BeginIndex0;
  }

  /** Get the index. This provides a read only copy of the index. */
  const IndexType
  GetIndex() const
  {
    IndexType indR(m_Image->GetBufferedRegion().GetIndex());

    indR[0] += m_Index0;
    typename BufferType::IndexType bufInd = m_BI.GetIndex();
    for (IndexValueType i = 1; i < VImageDimension; i++)
    {
      indR[i] = bufInd[i - 1];
    }
    return indR;
  }

  /** Sets the image index. No bounds checking is performed. */
  virtual void
  SetIndex(const IndexType & ind)
  {
    typename BufferType::IndexType bufInd;
    for (IndexValueType i = 1; i < VImageDimension; i++)
    {
      bufInd[i - 1] = ind[i];
    }
    m_BI.SetIndex(bufInd);
    SetIndexInternal(ind[0] - m_Image->GetBufferedRegion().GetIndex(0));
  }

  /** Get the region that this iterator walks. ImageConstIterators know the
   * beginning and the end of the region of the image to iterate over. */
  const RegionType
  GetRegion() const
  {
    RegionType r;

    r.SetIndex(0, m_BeginIndex0 + m_Image->GetBufferedRegion().GetIndex(0));
    r.SetSize(0, m_EndIndex0 - m_BeginIndex0);
    typename BufferType::RegionType ir = m_BI.GetRegion();
    for (IndexValueType i = 1; i < VImageDimension; i++)
    {
      r.SetIndex(i, ir.GetIndex(i - 1));
      r.SetSize(i, ir.GetSize(i - 1));
    }
    return r;
  }

  /** Get the image that this iterator walks. */
  const ImageType *
  GetImage() const
  {
    return m_Image.GetPointer();
  }

  /** Get the pixel value */
  PixelType
  Get() const
  {
    return Value();
  }

  /** Return a const reference to the pixel
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  const PixelType &
  Value() const
  {
    RLLine & line = const_cast<Self *>(this)->m_BI.Value();

    return line[m_RealIndex].second;
  }

  /** Move an iterator to the beginning of the region. "Begin" is
   * defined as the first pixel in the region. */
  void
  GoToBegin()
  {
    m_BI.GoToBegin();
    SetIndexInternal(m_BeginIndex0);
  }

  /** Move an iterator to the end of the region. "End" is defined as
   * one pixel past the last pixel of the region. */
  void
  GoToEnd()
  {
    m_BI.GoToEnd();
    m_Index0 = m_BeginIndex0;
  }

  /** Is the iterator at the beginning of the region? "Begin" is defined
   * as the first pixel in the region. */
  bool
  IsAtBegin() const
  {
    return m_Index0 == m_BeginIndex0 && m_BI.IsAtBegin();
  }

  /** Is the iterator at the end of the region? "End" is defined as one
   * pixel past the last pixel of the region. */
  bool
  IsAtEnd() const
  {
    return m_Index0 == m_BeginIndex0 && m_BI.IsAtEnd();
  }

protected: // made protected so other iterators can access
  /** Set the internal index, m_RealIndex and m_SegmentRemainder. */
  virtual void
  SetIndexInternal(const IndexValueType ind0)
  {
    m_Index0 = ind0;
    m_RunLengthLine = &m_BI.Value();

    CounterType   t = 0;
    SizeValueType x = 0;
    for (; x < (*m_RunLengthLine).size(); x++)
    {
      t += (*m_RunLengthLine)[x].first;
      if (t > m_Index0)
      {
        break;
      }
    }
    m_RealIndex = x;
    m_SegmentRemainder = t - m_Index0;
  } // SetIndexInternal

  typename ImageType::ConstWeakPointer m_Image;

  IndexValueType m_Index0; // index into the RLLine

  const RLLine * m_RunLengthLine;

  mutable SizeValueType  m_RealIndex;        // index into line's segment
  mutable IndexValueType m_SegmentRemainder; // how many pixels remain in current segment

  IndexValueType m_BeginIndex0; // index to first pixel in region in relation to buffer start
  IndexValueType m_EndIndex0;   // index to one pixel past last pixel in region in relation to buffer start
  BufferIterator m_BI;          // iterator over internal buffer image

  typename BufferType::Pointer m_Buffer;
};

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
class ImageConstIteratorWithIndex<RLEImage<TPixel, VImageDimension, CounterType>>
  : public ImageConstIterator<RLEImage<TPixel, VImageDimension, CounterType>>
{
  // just inherit constructors

public:
  /** Image type alias support. */
  using ImageType = RLEImage<TPixel, VImageDimension, CounterType>;

  using RegionType = typename itk::ImageConstIterator<RLEImage<TPixel, VImageDimension, CounterType>>::RegionType;

  void
  GoToReverseBegin()
  {
    this->m_BI.GoToReverseBegin();
    this->m_Index0 = this->m_EndIndex0 - 1;
    SetIndexInternal(this->m_Index0);
  }

  bool
  IsAtReverseEnd()
  {
    return this->m_BI.IsAtReverseEnd();
  }

  /** Default Constructor. Need to provide a default constructor since we
   * provide a copy constructor. */
  ImageConstIteratorWithIndex()
    : ImageConstIterator<ImageType>()
  {}

  /** Copy Constructor. The copy constructor is provided to make sure the
   * handle to the image is properly reference counted. */
  ImageConstIteratorWithIndex(const ImageConstIteratorWithIndex & it) { ImageConstIterator<ImageType>::operator=(it); }

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageConstIteratorWithIndex(const ImageType * ptr, const RegionType & region)
    : ImageConstIterator<ImageType>(ptr, region)
  {}
}; // no additional implementation required

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
class ImageConstIteratorWithOnlyIndex<RLEImage<TPixel, VImageDimension, CounterType>>
  : public ImageConstIteratorWithIndex<RLEImage<TPixel, VImageDimension, CounterType>>
{
  // just inherit constructors

public:
  /** Image type alias support. */
  using ImageType = RLEImage<TPixel, VImageDimension, CounterType>;

  using RegionType = typename itk::ImageConstIterator<RLEImage<TPixel, VImageDimension, CounterType>>::RegionType;

  /** Default Constructor. Need to provide a default constructor since we
   * provide a copy constructor. */
  ImageConstIteratorWithOnlyIndex()
    : ImageConstIterator<ImageType>()
  {}

  /** Copy Constructor. The copy constructor is provided to make sure the
   * handle to the image is properly reference counted. */
  ImageConstIteratorWithOnlyIndex(const ImageConstIteratorWithOnlyIndex & it)
  {
    ImageConstIterator<ImageType>::operator=(it);
  }

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageConstIteratorWithOnlyIndex(const ImageType * ptr, const RegionType & region)
    : ImageConstIterator<ImageType>(ptr, region)
  {}
}; // no additional implementation required
} // end namespace itk

#endif // itkRLEImageConstIterator_h
