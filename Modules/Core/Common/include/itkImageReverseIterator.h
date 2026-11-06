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
#ifndef itkImageReverseIterator_h
#define itkImageReverseIterator_h

#include "itkSize.h"
#include "itkImageIterator.h"
#include <memory>
#include <type_traits> // For remove_const_t.

namespace itk
{
/** \class ImageReverseIterator
 * \brief Multi-dimensional image iterator.
 *
 * ImageReverseIterator is a templated class to represent a
 * multi-dimensional iterator. ImageReverseIterator is templated over the
 * dimension of the image and the data type of the image.
 *
 * ImageReverseIterator is a base class for all the reverse image
 * iterators. It provides the basic construction and comparison
 * operations.  However, it does not provide mechanisms for moving the
 * iterator.  A subclass of ImageReverseIterator must be used to move
 * the iterator.
 *
 * ImageReverseIterator is a multi-dimensional iterator, requiring
 * more information be specified before the iterator can be used than
 * conventional iterators.  Whereas the std::vector::iterator from the
 * STL only needs to be passed a pointer to establish the iterator,
 * the multi-dimensional image iterator needs a pointer, the size of
 * the buffer, the size of the region, the start index of the buffer,
 * and the start index of the region. To gain access to this
 * information, ImageReverseIterator holds a reference to the image
 * over which it is traversing.
 *
 * ImageReverseIterator assumes a particular layout of the image
 * data. In particular, the data is arranged in a 1D array as if it
 * were [][][][slice][row][col] with Index[0] = col, Index[1] = row,
 * Index[2] = slice, etc.
 *
 * \par MORE INFORMATION
 * For a complete description of the ITK Image Iterators and their API, please
 * see the Iterators chapter in the ITK Software Guide.  The ITK Software Guide
 * is available in print and as a free .pdf download from https://www.itk.org.
 *
 * \ingroup ImageIterators
 *
 * \sa ImageConstIterator \sa ConditionalConstIterator
 * \sa ConstNeighborhoodIterator \sa ConstShapedNeighborhoodIterator
 * \sa ConstSliceIterator  \sa CorrespondenceDataStructureIterator
 * \sa FloodFilledFunctionConditionalConstIterator
 * \sa FloodFilledImageFunctionConditionalConstIterator
 * \sa FloodFilledImageFunctionConditionalIterator
 * \sa FloodFilledSpatialFunctionConditionalConstIterator
 * \sa FloodFilledSpatialFunctionConditionalIterator
 * \sa ImageConstIterator \sa ImageConstIteratorWithIndex
 * \sa ImageIterator \sa ImageIteratorWithIndex
 * \sa ImageLinearConstIteratorWithIndex  \sa ImageLinearIteratorWithIndex
 * \sa ImageRandomConstIteratorWithIndex  \sa ImageRandomIteratorWithIndex
 * \sa ImageRegionConstIterator \sa ImageRegionConstIteratorWithIndex
 * \sa ImageRegionExclusionConstIteratorWithIndex
 * \sa ImageRegionExclusionIteratorWithIndex
 * \sa ImageRegionIterator  \sa ImageRegionIteratorWithIndex
 * \sa ImageRegionReverseConstIterator  \sa ImageRegionReverseIterator
 * \sa ImageReverseConstIterator  \sa ImageReverseIterator
 * \sa ImageSliceConstIteratorWithIndex  \sa ImageSliceIteratorWithIndex
 * \sa NeighborhoodIterator \sa PathConstIterator  \sa PathIterator
 * \sa ShapedNeighborhoodIterator  \sa SliceIterator
 * \sa ImageConstIteratorWithIndex
 * \ingroup ITKCommon
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ImageReverseIterator
{
  template <typename>
  friend class ImageReverseIterator;

public:
  /** Standard class type aliases. */
  using Self = ImageReverseIterator;

  /** Dimension of the image the iterator walks.  This constant is needed so
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  static constexpr unsigned int ImageIteratorDimension = TImage::ImageDimension;

  /** \see LightObject::GetNameOfClass() */
  itkVirtualGetNameOfClassMacro(ImageReverseIterator);

  /** Index type alias support */
  using IndexType = typename TImage::IndexType;

  /** Size type alias support */
  using SizeType = typename TImage::SizeType;

  /** Offset type alias support */
  using OffsetType = typename TImage::OffsetType;

  /** Region type alias support */
  using RegionType = typename TImage::RegionType;

  /** Image type alias support */
  using ImageType = std::remove_const_t<TImage>;

  /** PixelContainer type alias support Used to refer to the container for
   * the pixel data. While this was already typedef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc. */
  using PixelContainer = typename TImage::PixelContainer;
  using PixelContainerPointer = typename PixelContainer::Pointer;

  /** Internal Pixel Type */
  using InternalPixelType = typename TImage::InternalPixelType;

  /** External Pixel Type */
  using PixelType = typename TImage::PixelType;

  /**  Accessor type that convert data between internal and external
   *  representations. */
  using AccessorType = typename TImage::AccessorType;

  /** Functor to choose the appropriate accessor. (for Image vs VectorImage) */
  using AccessorFunctorType = typename TImage::AccessorFunctorType;

  /** Default Constructor. Need to provide a default constructor since we
   * provide a copy constructor. */
  ImageReverseIterator()
    : m_Buffer(0)
    , m_PixelAccessor()
    , m_PixelAccessorFunctor()
  {
    m_PixelAccessorFunctor.SetBegin(m_Buffer);
  }

  /** Default Destructor. */
  virtual ~ImageReverseIterator() = default;

  /** Copy constructor and copy assignment. */
  ImageReverseIterator(const Self &) = default;
  Self &
  operator=(const Self &) = default;

  /** Conversion from a writable iterator to a read-only iterator. */
  template <typename TOtherImage, std::enable_if_t<detail::IsImageIteratorConvertible_v<TOtherImage, TImage>, int> = 0>
  ImageReverseIterator(const ImageReverseIterator<TOtherImage> & it)
    : m_Image(it.m_Image)
    , m_Region(it.m_Region)
    , m_Offset(it.m_Offset)
    , m_BeginOffset(it.m_BeginOffset)
    , m_EndOffset(it.m_EndOffset)
    , m_Buffer(it.m_Buffer)
    , m_PixelAccessor(it.m_PixelAccessor)
    , m_PixelAccessorFunctor(it.m_PixelAccessorFunctor)
    , m_SpanBeginOffset(it.m_SpanBeginOffset)
    , m_SpanEndOffset(it.m_SpanEndOffset)
  {
    m_PixelAccessorFunctor.SetBegin(m_Buffer);
  }

  /** Constructor establishes an iterator to walk a particular image and a particular region of that image. Initializes
   * the iterator at the begin of the region. */
  ImageReverseIterator(TImage * ptr, const RegionType & region)
    : m_Image(ptr)
    , m_Region(region)
    , m_Buffer(m_Image->GetBufferPointer())
  {


    // Compute the end offset, one pixel before the first pixel
    SizeValueType offset = m_Image->ComputeOffset(m_Region.GetIndex());
    m_EndOffset = offset - 1;

    // Compute the begin offset, the last pixel in the region
    IndexType ind(m_Region.GetIndex());
    SizeType  size(m_Region.GetSize());
    for (unsigned int i = 0; i < TImage::ImageDimension; ++i)
    {
      ind[i] += (size[i] - 1);
    }
    m_BeginOffset = m_Image->ComputeOffset(ind);
    m_Offset = m_BeginOffset;
    m_SpanBeginOffset = m_BeginOffset;
    m_SpanEndOffset = m_BeginOffset - static_cast<OffsetValueType>(m_Region.GetSize()[0]);

    m_PixelAccessor = ptr->GetPixelAccessor();
    m_PixelAccessorFunctor.SetPixelAccessor(m_PixelAccessor);
    m_PixelAccessorFunctor.SetBegin(m_Buffer);
  }

  /** Constructor that can be used to cast from an ImageConstIterator to an
   * ImageRegionReverseIterator. Many routines return an ImageConstIterator
   * but for a particular task, you may want an
   * ImageRegionReverseIterator.  Rather than provide overloaded APIs
   * that return different types of Iterators, itk returns
   * ImageConstIterators and uses constructors to cast from an
   * ImageConstIterator to a ImageRegionReverseIterator. */
  template <typename TOtherImage, std::enable_if_t<detail::IsImageIteratorConvertible_v<TOtherImage, TImage>, int> = 0>
  ImageReverseIterator(const ImageIterator<TOtherImage> & it)
    : m_Image(it.GetImage())
    , m_Region(it.GetRegion())
    , m_EndOffset(m_Image->ComputeOffset(m_Region.GetIndex()) - 1)
    , m_Buffer(m_Image->GetBufferPointer())
  {


    const IndexType ind = it.ComputeIndex();

    m_Offset = m_Image->ComputeOffset(ind);

    // Compute the end offset, one pixel before the first pixel


    // Compute the begin offset, the last pixel in the region
    IndexType regInd(m_Region.GetIndex());
    SizeType  regSize(m_Region.GetSize());
    for (unsigned int i = 0; i < TImage::ImageDimension; ++i)
    {
      regInd[i] += (regSize[i] - 1);
    }
    m_BeginOffset = m_Image->ComputeOffset(regInd);

    m_PixelAccessor = m_Image->GetPixelAccessor();
    m_PixelAccessorFunctor.SetPixelAccessor(m_PixelAccessor);
    m_PixelAccessorFunctor.SetBegin(m_Buffer);
    InitializeSpans();
  }

  /** operator= is provided to make sure the handle to the image is properly
   * reference counted. */
  template <typename TOtherImage, std::enable_if_t<detail::IsImageIteratorConvertible_v<TOtherImage, TImage>, int> = 0>
  Self &
  operator=(const ImageIterator<TOtherImage> & it)
  {
    m_Image = it.GetImage();
    m_Region = it.GetRegion();
    m_Buffer = m_Image->GetBufferPointer();

    IndexType ind = it.GetIndex();

    m_Offset = m_Image->ComputeOffset(ind);

    // Compute the end offset, one pixel before the first pixel
    m_EndOffset = m_Image->ComputeOffset(m_Region.GetIndex()) - 1;

    // Compute the begin offset, the last pixel in the region
    IndexType regInd(m_Region.GetIndex());
    SizeType  regSize(m_Region.GetSize());
    for (unsigned int i = 0; i < TImage::ImageDimension; ++i)
    {
      regInd[i] += (regSize[i] - 1);
    }
    m_BeginOffset = m_Image->ComputeOffset(regInd);

    m_PixelAccessor = m_Image->GetPixelAccessor();
    m_PixelAccessorFunctor.SetPixelAccessor(m_PixelAccessor);
    m_PixelAccessorFunctor.SetBegin(m_Buffer);
    InitializeSpans();
    return *this;
  }

  /** Get the dimension (size) of the index. */
  static unsigned int
  GetImageIteratorDimension()
  {
    return TImage::ImageDimension;
  }

  /** Comparison operator. Two iterators are the same if they "point to" the
   * same memory location */
  template <typename TOtherImage,
            std::enable_if_t<std::is_same_v<std::remove_const_t<TOtherImage>, ImageType>, int> = 0>
  bool
  operator==(const ImageReverseIterator<TOtherImage> & it) const
  {
    // two iterators are the same if they "point to" the same memory location
    return (m_Buffer + m_Offset) == (it.m_Buffer + it.m_Offset);
  }

#ifndef ITK_EXPERIMENTAL_CXX20_REWRITTEN_UNEQUAL_OPERATOR
  template <typename TOtherImage,
            std::enable_if_t<std::is_same_v<std::remove_const_t<TOtherImage>, ImageType>, int> = 0>
  bool
  operator!=(const ImageReverseIterator<TOtherImage> & it) const
  {
    return !(this->operator==(it));
  }
#endif


  /** Get the index. This provides a read only reference to the index.
   * This causes the index to be calculated from pointer arithmetic and is
   * therefore an expensive operation.
   * \sa SetIndex */
  const IndexType
  GetIndex()
  {
    return m_Image->ComputeIndex(m_Offset);
  }

  /** Set the index. No bounds checking is performed.
   * \sa GetIndex */
  virtual void
  SetIndex(const IndexType & ind)
  {
    m_Offset = m_Image->ComputeOffset(ind);
    InitializeSpans();
  }

  /** Get the region that this iterator walks. ImageReverseConstIterators know the
   * beginning and the end of the region of the image to iterate over. */
  [[nodiscard]] const RegionType &
  GetRegion() const
  {
    return m_Region;
  }

  /** Get the pixel value */
  [[nodiscard]] const PixelType
  Get() const
  {
    return m_PixelAccessorFunctor.Get(*(m_Buffer + m_Offset));
  }

  /** Set the pixel value. Available only when TImage is not const. */
  template <typename T = TImage, std::enable_if_t<std::is_same_v<T, TImage> && !std::is_const_v<T>, int> = 0>
  void
  Set(const PixelType & value) const
  {
    this->m_PixelAccessorFunctor.Set(*(this->m_Buffer + this->m_Offset), value);
  }

  /** Return a const reference to the pixel
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  [[nodiscard]] const PixelType &
  Value() const
  {
    return *(m_Buffer + m_Offset);
  }

  /** Return a reference to the pixel
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. Available only when TImage is not const. */
  template <typename T = TImage, std::enable_if_t<std::is_same_v<T, TImage> && !std::is_const_v<T>, int> = 0>
  PixelType &
  Value()
  {
    return *(m_Buffer + m_Offset);
  }

  /** Move an iterator to the beginning of the region. "Begin" for a reverse
   * iterator is the last pixel in the region. */
  void
  GoToBegin()
  {
    m_Offset = m_BeginOffset;
    m_SpanBeginOffset = m_BeginOffset;
    m_SpanEndOffset = m_BeginOffset - static_cast<OffsetValueType>(m_Region.GetSize()[0]);
  }

  /** Move an iterator to the end of the region. "End" for a reverse iterator
   * is defined as one pixel before the first pixel in the region. */
  void
  GoToEnd()
  {
    m_Offset = m_EndOffset;
    m_SpanEndOffset = m_EndOffset;
    m_SpanBeginOffset = m_SpanEndOffset + static_cast<OffsetValueType>(m_Region.GetSize()[0]);
  }

  /** Is the iterator at the beginning of the (reverse) region? "Begin" for
   * a reverse iterator is the last pixel in the region. */
  [[nodiscard]] bool
  IsAtBegin() const
  {
    return m_Offset == m_BeginOffset;
  }

  /** Is the iterator at the end of the (reverse) region? "End" for a reverse
   * iterator is one pixel before the first pixel in the region. */
  [[nodiscard]] bool
  IsAtEnd() const
  {
    return m_Offset == m_EndOffset;
  }

  /** iterator's index.  For a reverse iterator, this moves backwards
   * through the region.  This operator will constrain the iterator
   * within the region (i.e. the iterator will automatically wrap from
   * the start of the row of the region to the end of the previous row
   * of the region) up until the iterator tries to moves before the
   * first pixel of the region.  Here, the iterator will be set to be
   * one pixel before the start of the region.
   * \sa operator++(int). */
  Self &
  operator++()
  {
    if (--this->m_Offset <= m_SpanEndOffset)
    {
      // We have past the beginning of the span (row), need to wrap around.

      // First move forward one pixel, because we are going to use a different
      // algorithm to compute the next pixel
      this->m_Offset++;

      // Get the index of the first pixel on the span (row)
      IndexType ind = this->m_Image->ComputeIndex(static_cast<OffsetValueType>(this->m_Offset));

      const IndexType & startIndex = this->m_Region.GetIndex();
      const SizeType &  size = this->m_Region.GetSize();

      // Decrement along a row, then wrap at the beginning of the region row.

      // Check to see if we are past the first pixel in the region
      // Note that --ind[0] moves to the previous pixel along the row.
      bool done = (--ind[0] == startIndex[0] - 1);
      for (unsigned int i = 1; done && i < this->ImageIteratorDimension; ++i)
      {
        done = (ind[i] == startIndex[i]);
      }

      // if the iterator is outside the region (but not past region begin) then
      // we need to wrap around the region
      unsigned int dim = 0;
      if (!done)
      {
        while ((dim < this->ImageIteratorDimension - 1) && (ind[dim] < startIndex[dim]))
        {
          ind[dim] = startIndex[dim] + static_cast<OffsetValueType>(size[dim]) - 1;
          ind[++dim]--;
        }
      }
      this->m_Offset = this->m_Image->ComputeOffset(ind);
      m_SpanBeginOffset = this->m_Offset;
      m_SpanEndOffset = m_SpanBeginOffset - static_cast<OffsetValueType>(size[0]);
    }
    return *this;
  }

  /** Decrement (prefix) the fastest moving dimension of the iterator's index.
   * For a reverse iterator, this moves forward through the region.
   * This operator will constrain the iterator within the region (i.e. the
   * iterator will automatically wrap from the end of the row of the region
   * to the start of the next row of the region) up until the iterator
   * tries to moves past the last pixel of the region.  Here, the iterator
   * will be set to be one pixel past the end of the region.
   * \sa operator--(int) */
  Self &
  operator--()
  {
    if (++this->m_Offset >= m_SpanBeginOffset)
    {
      // We have reached the end of the span (row), need to wrap around.

      // First back up one pixel, because we are going to use a different
      // algorithm to compute the next pixel
      --this->m_Offset;

      // Get the index of the last pixel on the span (row)
      IndexType ind = this->m_Image->ComputeIndex(static_cast<OffsetValueType>(this->m_Offset));

      const typename ImageIterator<TImage>::IndexType & startIndex = this->m_Region.GetIndex();
      const typename ImageIterator<TImage>::SizeType &  size = this->m_Region.GetSize();

      // Increment along a row, then wrap at the end of the region row.
      // Check to see if we are past the last pixel in the region
      // Note that ++ind[0] moves to the next pixel along the row.
      bool done = (++ind[0] == startIndex[0] + static_cast<OffsetValueType>(size[0]));
      for (unsigned int i = 1; done && i < this->ImageIteratorDimension; ++i)
      {
        done = (ind[i] == startIndex[i] + static_cast<OffsetValueType>(size[i]) - 1);
      }

      // if the iterator is outside the region (but not past region end) then
      // we need to wrap around the region
      unsigned int dim = 0;
      if (!done)
      {
        while ((dim < this->ImageIteratorDimension - 1) &&
               (ind[dim] > startIndex[dim] + static_cast<OffsetValueType>(size[dim]) - 1))
        {
          ind[dim] = startIndex[dim];
          ind[++dim]++;
        }
      }
      this->m_Offset = this->m_Image->ComputeOffset(ind);
      m_SpanBeginOffset = this->m_Offset;
      m_SpanEndOffset = this->m_Offset - static_cast<OffsetValueType>(size[0]);
    }
    return *this;
  }

protected: // made protected so other iterators can access
  WeakPointer<TImage> m_Image{};

  RegionType m_Region{}; // region to iterate over

  SizeValueType m_Offset{};
  SizeValueType m_BeginOffset{}; // offset to last pixel in region
  SizeValueType m_EndOffset{};   // offset to one pixel before first pixel

  std::conditional_t<std::is_const_v<TImage>, const InternalPixelType, InternalPixelType> * m_Buffer{};

  AccessorType        m_PixelAccessor{};
  AccessorFunctorType m_PixelAccessorFunctor{};

  SizeValueType m_SpanBeginOffset{}; // offset to last pixel in the row
  SizeValueType m_SpanEndOffset{};   // offset to one pixel before the row

private:
  void
  InitializeSpans()
  {
    const IndexType ind = this->GetIndex();
    const auto      rowSize = static_cast<OffsetValueType>(this->m_Region.GetSize()[0]);
    m_SpanBeginOffset = this->m_Offset + rowSize - 1 - (ind[0] - this->m_Region.GetIndex()[0]);
    m_SpanEndOffset = m_SpanBeginOffset - rowSize;
  }
};

// Deduction guide for class template argument deduction (CTAD).
template <typename TImage>
ImageReverseIterator(SmartPointer<TImage>, const typename TImage::RegionType &) -> ImageReverseIterator<TImage>;

/** \class ImageReverseConstIterator
 * \brief Read-only ImageReverseIterator: ImageReverseConstIterator<TImage> behaves as
 * ImageReverseIterator<const TImage>.
 *
 * It is a derived class rather than an alias template so that it supports class template argument deduction.
 *
 * \ingroup ImageIterators
 * \ingroup ITKCommon
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ImageReverseConstIterator : public ImageReverseIterator<std::add_const_t<TImage>>
{
public:
  /** Standard class type alias. */
  using Self = ImageReverseConstIterator;
  using Superclass = ImageReverseIterator<std::add_const_t<TImage>>;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(ImageReverseConstIterator);

  using Superclass::Superclass;

  ImageReverseConstIterator() = default;

  /** Conversion from ImageReverseIterator<const TImage>. */
  ImageReverseConstIterator(const Superclass & it)
    : Superclass(it)
  {}
};

// Deduction guides for class template argument deduction (CTAD).
template <typename TImage>
ImageReverseConstIterator(const TImage *, const typename TImage::RegionType &) -> ImageReverseConstIterator<TImage>;

template <typename TImage>
ImageReverseConstIterator(SmartPointer<TImage>, const typename TImage::RegionType &)
  -> ImageReverseConstIterator<std::remove_const_t<TImage>>;

} // end namespace itk

#endif
