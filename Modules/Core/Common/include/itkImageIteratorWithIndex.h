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
#ifndef itkImageIteratorWithIndex_h
#define itkImageIteratorWithIndex_h

#include "itkImageIterator.h"
#include <memory>
#include <type_traits>

namespace itk
{
/**
 * \class ImageIteratorWithIndex
 * \brief A base class for multi-dimensional iterators templated over image
 * type that are designed to efficiently keep track of the iterator
 * position.
 *
 * The constness of TImage selects the access: ImageIteratorWithIndex<const TImage>
 * (equivalently ImageConstIteratorWithIndex<TImage>) is read-only. See
 * ImageConstIteratorWithIndex for more information.
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
 *
 * \ingroup ITKCommon
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ImageIteratorWithIndex
{
  template <typename>
  friend class ImageIteratorWithIndex;

public:
  /** Standard class type aliases. */
  using Self = ImageIteratorWithIndex;

  /** Dimension of the image that the iterator walks.  This constant is needed so
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  static constexpr unsigned int ImageDimension = TImage::ImageDimension;
  static constexpr unsigned int ImageIteratorDimension = ImageDimension;

  /** Index type alias support */
  using IndexType = typename TImage::IndexType;
  using IndexValueType = typename IndexType::IndexValueType;

  /** Size type alias support */
  using SizeType = typename TImage::SizeType;
  using SizeValueType = typename SizeType::SizeValueType;

  /** Region type alias support */
  using RegionType = typename TImage::RegionType;

  /** Image type alias support */
  using ImageType = std::remove_const_t<TImage>;

  /** PixelContainer type alias support Used to refer to the container for
   * the pixel data. While this was already typedef'ed in the superclass,
   * it needs to be redone here for this subclass to compile properly with gcc. */
  using PixelContainer = typename TImage::PixelContainer;
  using PixelContainerPointer = typename PixelContainer::Pointer;

  /** Internal Pixel Type */
  using InternalPixelType = typename TImage::InternalPixelType;

  /** External Pixel Type */
  using PixelType = typename TImage::PixelType;

  /**  Accessor type that converts data between internal and external
   *  representations. */
  using AccessorType = typename TImage::AccessorType;
  using AccessorFunctorType = typename TImage::AccessorFunctorType;

  /** Type of the Offset taken from the image */
  using OffsetType = typename TImage::OffsetType;
  using OffsetValueType = typename OffsetType::OffsetValueType;

  /** Default Constructor. Need to provide a default constructor since we
   * provide a copy constructor. */
  ImageIteratorWithIndex() = default;

  /** Copy Constructor. The copy constructor is provided to make sure the
   * handle to the image is properly reference counted. */
  ImageIteratorWithIndex(const Self & it);

  /** Constructor establishes an iterator to walk a particular image and a particular region of that image. Initializes
   * the iterator at the begin of the region. */
  ImageIteratorWithIndex(TImage * ptr, const RegionType & region);

  /** Conversion from a writable iterator to a read-only iterator. */
  template <typename TOtherImage, std::enable_if_t<detail::IsImageIteratorConvertible_v<TOtherImage, TImage>, int> = 0>
  ImageIteratorWithIndex(const ImageIteratorWithIndex<TOtherImage> & it)
    : m_Image(it.m_Image)
    , m_PositionIndex(it.m_PositionIndex)
    , m_BeginIndex(it.m_BeginIndex)
    , m_EndIndex(it.m_EndIndex)
    , m_Region(it.m_Region)
    , m_Position(it.m_Position)
    , m_Begin(it.m_Begin)
    , m_End(it.m_End)
    , m_Remaining(it.m_Remaining)
    , m_PixelAccessor(it.m_PixelAccessor)
    , m_PixelAccessorFunctor(it.m_PixelAccessorFunctor)
  {
    std::copy_n(it.m_OffsetTable, ImageDimension + 1, m_OffsetTable);
    m_PixelAccessorFunctor.SetBegin(m_Image->GetBufferPointer());
  }

  /** Default Destructor. */
  virtual ~ImageIteratorWithIndex() = default;

  /** operator= is provided to make sure the handle to the image is properly
   * reference counted. */
  Self &
  operator=(const Self & it);

  /** Get the dimension (size) of the index. */
  static constexpr unsigned int
  GetImageDimension()
  {
    return ImageDimension;
  }

  /** Comparison operators compare the positions of the iterators. Iterators over the same image type compare,
   * regardless of their constness. */
#if defined(__cpp_concepts) && defined(__cpp_impl_three_way_comparison)
  template <typename TOtherImage>
    requires std::is_same_v<std::remove_const_t<TOtherImage>, ImageType>
  [[nodiscard]] bool
  operator==(const ImageIteratorWithIndex<TOtherImage> & it) const
  {
    return m_Position == it.m_Position;
  }

  template <typename TOtherImage>
    requires std::is_same_v<std::remove_const_t<TOtherImage>, ImageType>
  [[nodiscard]] auto
  operator<=>(const ImageIteratorWithIndex<TOtherImage> & it) const
  {
    return m_Position <=> it.m_Position;
  }
#else
  template <typename TOtherImage,
            std::enable_if_t<std::is_same_v<std::remove_const_t<TOtherImage>, ImageType>, int> = 0>
  [[nodiscard]] bool
  operator==(const ImageIteratorWithIndex<TOtherImage> & it) const
  {
    return m_Position == it.m_Position;
  }

  template <typename TOtherImage,
            std::enable_if_t<std::is_same_v<std::remove_const_t<TOtherImage>, ImageType>, int> = 0>
  [[nodiscard]] bool
  operator!=(const ImageIteratorWithIndex<TOtherImage> & it) const
  {
    return !(this->operator==(it));
  }

  template <typename TOtherImage,
            std::enable_if_t<std::is_same_v<std::remove_const_t<TOtherImage>, ImageType>, int> = 0>
  [[nodiscard]] bool
  operator<=(const ImageIteratorWithIndex<TOtherImage> & it) const
  {
    return m_Position <= it.m_Position;
  }

  template <typename TOtherImage,
            std::enable_if_t<std::is_same_v<std::remove_const_t<TOtherImage>, ImageType>, int> = 0>
  [[nodiscard]] bool
  operator<(const ImageIteratorWithIndex<TOtherImage> & it) const
  {
    return m_Position < it.m_Position;
  }

  template <typename TOtherImage,
            std::enable_if_t<std::is_same_v<std::remove_const_t<TOtherImage>, ImageType>, int> = 0>
  [[nodiscard]] bool
  operator>=(const ImageIteratorWithIndex<TOtherImage> & it) const
  {
    return m_Position >= it.m_Position;
  }

  template <typename TOtherImage,
            std::enable_if_t<std::is_same_v<std::remove_const_t<TOtherImage>, ImageType>, int> = 0>
  [[nodiscard]] bool
  operator>(const ImageIteratorWithIndex<TOtherImage> & it) const
  {
    return m_Position > it.m_Position;
  }
#endif

  /** Get the index. This provides a read only reference to the index.
   * \sa SetIndex */
  [[nodiscard]] const IndexType &
  GetIndex() const
  {
    return m_PositionIndex;
  }

  /** Get the region that this iterator walks. ImageIterators know the
   * beginning and the end of the region of the image to iterate over. */
  [[nodiscard]] const RegionType &
  GetRegion() const
  {
    return m_Region;
  }

  /** Set the index. No bounds checking is performed.
   * \sa GetIndex */
  void
  SetIndex(const IndexType & ind)
  {
    m_Position = m_Image->GetBufferPointer() + m_Image->ComputeOffset(ind);
    m_PositionIndex = ind;
  }

  /** Get the pixel value */
  [[nodiscard]] PixelType
  Get() const
  {
    return m_PixelAccessorFunctor.Get(*m_Position);
  }

  /** Set the pixel value. Available only when TImage is not const. */
#ifdef __cpp_concepts
  void
  Set(const PixelType & value) const
    requires(!std::is_const_v<TImage>)
#else
  template <typename T = TImage, std::enable_if_t<std::is_same_v<T, TImage> && !std::is_const_v<T>, int> = 0>
  void
  Set(const PixelType & value) const
#endif
  {
    m_PixelAccessorFunctor.Set(*m_Position, value);
  }

  /** Return a const reference to the pixel.
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  [[nodiscard]] const PixelType &
  Value() const
  {
    return *m_Position;
  }

  /** Return a reference to the pixel. Available only when TImage is not const.
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
#ifdef __cpp_concepts
  PixelType &
  Value()
    requires(!std::is_const_v<TImage>)
#else
  template <typename T = TImage, std::enable_if_t<std::is_same_v<T, TImage> && !std::is_const_v<T>, int> = 0>
  PixelType &
  Value()
#endif
  {
    return *m_Position;
  }

  /** Move an iterator to the beginning of the region. */
  void
  GoToBegin();

  /** Move an iterator to the End of the region. */
  void
  GoToReverseBegin();

  /** Is the iterator at the beginning of the region? */
  [[nodiscard]] bool
  IsAtReverseEnd() const
  {
    return !m_Remaining;
  }

  /** Is the iterator at the end of the region? */
  [[nodiscard]] bool
  IsAtEnd() const
  {
    return !m_Remaining;
  }

  /** Are there data remaining in the region ? */
  bool
  Remaining()
  {
    return m_Remaining;
  }

protected: // made protected so other iterators can access
  /** InternalPixelType, const-qualified when TImage is const. */
  using QualifiedInternalPixelType =
    std::conditional_t<std::is_const_v<TImage>, const InternalPixelType, InternalPixelType>;

  WeakPointer<TImage> m_Image{};

  IndexType m_PositionIndex{ { 0 } }; // Index where we currently are
  IndexType m_BeginIndex{ { 0 } };    // Index to start iterating over
  IndexType m_EndIndex{ { 0 } };      // Index to finish iterating:
                                      // one pixel past the end of each
                                      // row, col, slice, etc....

  RegionType m_Region{}; // region to iterate over

  OffsetValueType m_OffsetTable[ImageDimension + 1]{};

  QualifiedInternalPixelType * m_Position{ nullptr };
  QualifiedInternalPixelType * m_Begin{ nullptr };
  QualifiedInternalPixelType * m_End{ nullptr };

  bool m_Remaining{ false };

  AccessorType        m_PixelAccessor{};
  AccessorFunctorType m_PixelAccessorFunctor{};
};

// Deduction guide for class template argument deduction (CTAD).
template <typename TImage>
ImageIteratorWithIndex(SmartPointer<TImage>, const typename TImage::RegionType &) -> ImageIteratorWithIndex<TImage>;

/** \class ImageConstIteratorWithIndex
 * \brief A base class for multi-dimensional iterators templated over image
 * type that are designed to efficiently keep track of the iterator
 * position.
 *
 * ImageConstIteratorWithIndex is a templated class to represent a
 * multi-dimensional iterator, and is templated over both the dimension
 * and data type of the image.
 *
 * ImageConstIteratorWithIndex<TImage> is the read-only ImageIteratorWithIndex<const TImage>.
 *
 * ImageIteratorWithIndex is a base class for the "WithIndex" family of
 * ITK image iterators, which are designed to efficiently keep track of the
 * image index position of the iterator during increment and decrement
 * operations.  This iterator is a base class and provides only the basic
 * construction and comparison operations. It does not provide mechanisms for
 * moving the iterator.  A subclass of ImageConstIteratorWithIndex must be used
 * to move the iterator.
 *
 * ImageConstIteratorWithIndex is a multi-dimensional iterator, requiring
 * more information be specified before the iterator can be used than
 * conventional iterators.  Whereas the std::vector::iterator from the
 * STL only needs to be passed a pointer to establish the iterator,
 * the multi-dimensional image iterator needs a pointer, the size of
 * the buffer, the size of the region, the start index of the buffer,
 * and the start index of the region. To gain access to this
 * information, ImageConstIteratorWithIndex holds a reference to the image
 * over which it is traversing.
 *
 * ImageConstIteratorWithIndex assumes a particular layout of the image
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
 *
 * \ingroup ImageIterators
 * \ingroup ITKCommon
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ImageConstIteratorWithIndex : public ImageIteratorWithIndex<std::add_const_t<TImage>>
{
public:
  /** Standard class type alias. */
  using Self = ImageConstIteratorWithIndex;
  using Superclass = ImageIteratorWithIndex<std::add_const_t<TImage>>;

  using Superclass::Superclass;

  ImageConstIteratorWithIndex() = default;

  /** Conversion from ImageIteratorWithIndex<const TImage>. */
  ImageConstIteratorWithIndex(const Superclass & it)
    : Superclass(it)
  {}
};

// Deduction guides for class template argument deduction (CTAD).
template <typename TImage>
ImageConstIteratorWithIndex(const TImage *, const typename TImage::RegionType &) -> ImageConstIteratorWithIndex<TImage>;

template <typename TImage>
ImageConstIteratorWithIndex(SmartPointer<TImage>, const typename TImage::RegionType &)
  -> ImageConstIteratorWithIndex<std::remove_const_t<TImage>>;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageIteratorWithIndex.hxx"
#endif

#endif
