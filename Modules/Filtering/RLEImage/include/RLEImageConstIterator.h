#ifndef RLEImageConstIterator_h
#define RLEImageConstIterator_h

#include "itkImage.h"
#include "itkIndex.h"
#include "itkNumericTraits.h"
#include "itkRLEImage.h"
#include "itkImageConstIterator.h"
#include "itkImageConstIteratorWithIndex.h"
#include "itkImageConstIteratorWithOnlyIndex.h"
#include "itkImageRegionIterator.h"

class MultiLabelMeshPipeline;

namespace itk
{
/** \class ImageConstIterator
 * \brief A multi-dimensional image iterator templated over image type.
 */

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
class ImageConstIterator<RLEImage<TPixel, VImageDimension, CounterType>>
{
  friend class ::MultiLabelMeshPipeline;

public:
  /** Standard class typedefs. */
  typedef ImageConstIterator Self;

  /** Dimension of the image the iterator walks.  This constant is needed so
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  itkStaticConstMacro(ImageIteratorDimension, unsigned int, VImageDimension);

  /** Run-time type information (and related methods). */
  itkTypeMacroNoParent(ImageConstIterator);

  /** Image typedef support. */
  typedef itk::RLEImage<TPixel, VImageDimension, CounterType> ImageType;

  /** Run-Length Line (we iterate along it). */
  typedef typename ImageType::RLLine RLLine;

  /** Buffer Type used. */
  typedef typename ImageType::BufferType BufferType;

  /** Type for the internal buffer iterator. */
  typedef ImageRegionIterator<BufferType> BufferIterator;

  /** Index typedef support. */
  typedef typename ImageType::IndexType IndexType;

  /** Index typedef support. */
  typedef typename ImageType::IndexValueType IndexValueType;

  /** Size typedef support. */
  typedef typename ImageType::SizeType SizeType;

  /** Offset typedef support. */
  typedef typename ImageType::OffsetType OffsetType;

  /** Region typedef support. */
  typedef typename ImageType::RegionType RegionType;

  /** Internal Pixel Type */
  typedef typename ImageType::InternalPixelType InternalPixelType;

  /** External Pixel Type */
  typedef typename ImageType::PixelType PixelType;

  /** Default Constructor. Need to provide a default constructor since we
   * provide a copy constructor. */
  ImageConstIterator()
    : m_Buffer(0)
    , rlLine(0)
  {
    m_Image = ITK_NULLPTR;
    m_Index0 = 0;
    m_BeginIndex0 = 0;
    m_EndIndex0 = 0;
    realIndex = 0;
    segmentRemainder = 0;
  }

  /** Default Destructor. */
  virtual ~ImageConstIterator() {}

  /** Copy Constructor. The copy constructor is provided to make sure the
   * handle to the image is properly reference counted. */
  ImageConstIterator(const Self & it)
    : m_Buffer(const_cast<ImageType *>(it.GetImage())->GetBuffer())
  {
    rlLine = it.rlLine;
    m_Image = it.m_Image; // copy the smart pointer
    m_Index0 = it.m_Index0;
    this->bi = it.bi;

    realIndex = it.realIndex;
    segmentRemainder = it.segmentRemainder;
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
      rlLine = it.rlLine;
      m_Image = it.m_Image; // copy the smart pointer
      m_Index0 = it.m_Index0;
      bi = it.bi;

      realIndex = it.realIndex;
      segmentRemainder = it.segmentRemainder;
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

    bi = BufferIterator(m_Buffer, ImageType::truncateRegion(region));
    m_Index0 = region.GetIndex(0);
    m_BeginIndex0 = m_Index0 - m_Image->GetBufferedRegion().GetIndex(0);
    m_EndIndex0 = m_BeginIndex0 + region.GetSize(0);
    SetIndexInternal(m_BeginIndex0); // sets realIndex and segmentRemainder
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
    return bi != it.bi || m_Index0 + m_BeginIndex0 != it.m_Index0 + it.m_BeginIndex0;
  }

  /** Comparison operator. Two iterators are the same if they "point to" the
   * same memory location */
  bool
  operator==(const Self & it) const
  {
    return bi == it.bi && m_Index0 + m_BeginIndex0 == it.m_Index0 + it.m_BeginIndex0;
  }

  /** Comparison operator. An iterator is "less than" another if it "points to"
   * a lower memory location. */
  bool
  operator<=(const Self & it) const
  {
    if (bi < it.bi)
      return true;
    else if (bi > it.bi)
      return false;
    return m_Index0 + m_BeginIndex0 <= it.m_Index0 + it.m_BeginIndex0;
  }

  /** Comparison operator. An iterator is "less than" another if it "points to"
   * a lower memory location. */
  bool
  operator<(const Self & it) const
  {
    if (bi < it.bi)
      return true;
    else if (bi > it.bi)
      return false;
    return m_Index0 + m_BeginIndex0 < it.m_Index0 + it.m_BeginIndex0;
  }

  /** Comparison operator. An iterator is "greater than" another if it
   * "points to" a higher location. */
  bool
  operator>=(const Self & it) const
  {
    if (bi > it.bi)
      return true;
    else if (bi < it.bi)
      return false;
    return m_Index0 + m_BeginIndex0 >= it.m_Index0 + it.m_BeginIndex0;
  }

  /** Comparison operator. An iterator is "greater than" another if it
   * "points to" a higher location. */
  bool
  operator>(const Self & it) const
  {
    if (bi > it.bi)
      return true;
    else if (bi < it.bi)
      return false;
    return m_Index0 + m_BeginIndex0 > it.m_Index0 + it.m_BeginIndex0;
  }

  /** Get the index. This provides a read only copy of the index. */
  const IndexType
  GetIndex() const
  {
    IndexType indR(m_Image->GetBufferedRegion().GetIndex());
    indR[0] += m_Index0;
    typename BufferType::IndexType bufInd = bi.GetIndex();
    for (IndexValueType i = 1; i < VImageDimension; i++)
      indR[i] = bufInd[i - 1];
    return indR;
  }

  /** Sets the image index. No bounds checking is performed. */
  virtual void
  SetIndex(const IndexType & ind)
  {
    typename BufferType::IndexType bufInd;
    for (IndexValueType i = 1; i < VImageDimension; i++)
      bufInd[i - 1] = ind[i];
    bi.SetIndex(bufInd);
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
    typename BufferType::RegionType ir = bi.GetRegion();
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
  Get(void) const
  {
    return Value();
  }

  /** Return a const reference to the pixel
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  const PixelType &
  Value(void) const
  {
    RLLine & line = const_cast<Self *>(this)->bi.Value();
    return line[realIndex].second;
  }

  /** Move an iterator to the beginning of the region. "Begin" is
   * defined as the first pixel in the region. */
  void
  GoToBegin()
  {
    bi.GoToBegin();
    SetIndexInternal(m_BeginIndex0);
  }

  /** Move an iterator to the end of the region. "End" is defined as
   * one pixel past the last pixel of the region. */
  void
  GoToEnd()
  {
    bi.GoToEnd();
    m_Index0 = m_BeginIndex0;
  }

  /** Is the iterator at the beginning of the region? "Begin" is defined
   * as the first pixel in the region. */
  bool
  IsAtBegin(void) const
  {
    return m_Index0 == m_BeginIndex0 && bi.IsAtBegin();
  }

  /** Is the iterator at the end of the region? "End" is defined as one
   * pixel past the last pixel of the region. */
  bool
  IsAtEnd(void) const
  {
    return m_Index0 == m_BeginIndex0 && bi.IsAtEnd();
  }

protected: // made protected so other iterators can access
  /** Set the internal index, realIndex and segmentRemainder. */
  virtual void
  SetIndexInternal(const IndexValueType ind0)
  {
    m_Index0 = ind0;
    rlLine = &bi.Value();

    CounterType   t = 0;
    SizeValueType x = 0;

    for (; x < (*rlLine).size(); x++)
    {
      t += (*rlLine)[x].first;
      if (t > m_Index0)
        break;
    }
    realIndex = x;
    segmentRemainder = t - m_Index0;
  }

  typename ImageType::ConstWeakPointer m_Image;

  IndexValueType m_Index0; // index into the RLLine

  const RLLine * rlLine;

  mutable IndexValueType realIndex;        // index into line's segment
  mutable IndexValueType segmentRemainder; // how many pixels remain in current segment

  IndexValueType m_BeginIndex0; // index to first pixel in region in relation to buffer start
  IndexValueType m_EndIndex0;   // index to one pixel past last pixel in region in relation to buffer start

  BufferIterator               bi; // iterator over internal buffer image
  typename BufferType::Pointer m_Buffer;
};

template <typename TPixel, unsigned int VImageDimension, typename CounterType>
class ImageConstIteratorWithIndex<RLEImage<TPixel, VImageDimension, CounterType>>
  : public ImageConstIterator<RLEImage<TPixel, VImageDimension, CounterType>>
{
  // just inherit constructors
public:
  /** Image typedef support. */
  typedef itk::RLEImage<TPixel, VImageDimension, CounterType> ImageType;

  typedef typename itk::ImageConstIterator<RLEImage<TPixel, VImageDimension, CounterType>>::RegionType RegionType;

  void
  GoToReverseBegin()
  {
    this->bi.GoToReverseBegin();
    this->m_Index0 = this->m_EndIndex0 - 1;
    SetIndexInternal(this->m_Index0);
  }

  bool
  IsAtReverseEnd()
  {
    return this->bi.IsAtReverseEnd();
  }

  /** Default Constructor. Need to provide a default constructor since we
   * provide a copy constructor. */
  ImageConstIteratorWithIndex()
    : ImageConstIterator<ImageType>()
  {}

  /** Copy Constructor. The copy constructor is provided to make sure the
   * handle to the image is properly reference counted. */
  ImageConstIteratorWithIndex(const ImageConstIteratorWithIndex & it)
  {
    this->ImageConstIterator<ImageType>::operator=(it);
  }

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
  /** Image typedef support. */
  typedef itk::RLEImage<TPixel, VImageDimension, CounterType> ImageType;

  typedef typename itk::ImageConstIterator<RLEImage<TPixel, VImageDimension, CounterType>>::RegionType RegionType;

  /** Default Constructor. Need to provide a default constructor since we
   * provide a copy constructor. */
  ImageConstIteratorWithOnlyIndex()
    : ImageConstIterator<ImageType>()
  {}


  /** Copy Constructor. The copy constructor is provided to make sure the
   * handle to the image is properly reference counted. */
  ImageConstIteratorWithOnlyIndex(const ImageConstIteratorWithOnlyIndex & it)
  {
    this->ImageConstIterator<ImageType>::operator=(it);
  }

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageConstIteratorWithOnlyIndex(const ImageType * ptr, const RegionType & region)
    : ImageConstIterator<ImageType>(ptr, region)
  {}
}; // no additional implementation required

} // end namespace itk

#endif // RLEImageConstIterator_h
