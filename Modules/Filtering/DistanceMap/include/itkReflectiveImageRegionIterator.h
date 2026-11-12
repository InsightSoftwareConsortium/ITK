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
#ifndef itkReflectiveImageRegionIterator_h
#define itkReflectiveImageRegionIterator_h

#include "itkImageIteratorWithIndex.h"
#include <algorithm>

namespace itk
{
/**
 * \class ReflectiveImageRegionIterator
 * \brief Multi-dimensional image iterator which only walks a region.
 *
 * ReflectiveImageRegionIterator is a templated class to represent a
 * multi-dimensional iterator. ReflectiveImageRegionIterator is templated
 * over the image type. ReflectiveImageRegionIterator is constrained to
 * walk only within the specified region.
 *
 * ReflectiveImageRegionIterator will perform two passes over the image
 * along each dimension. It is useful for algorithms that require to
 * go back and forth (once) over the data.
 *
 * By setting the BeginOffset and EndOffset parameters, you can arrange for
 * the starting point, when going forwards, to be different from the ending
 * point, when going backwards. Along dimension d, when going forwards the
 * iterator traverses the interval [BeginIndex[d]+BeginOffset[d], EndIndex[d]).
 * When going back, it traverses the interval (EndIndex[d]-EndOffset[d],
 * BeginIndex[d]].  Setting both offsets to (1, ..., 1) enables the
 * DanielssonDistanceMapImageFilter to process the entire image,
 * without encountering boundary conditions, rather
 * than removing a one-pixel border.
 *
 * \sa DanielssonDistanceMapImageFilter
 *
 * \ingroup Iterators
 * \ingroup ITKDistanceMap
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ReflectiveImageRegionIterator : public ImageIteratorWithIndex<TImage>
{
  template <typename>
  friend class ReflectiveImageRegionIterator;

public:
  /** Standard class type aliases. */
  using Self = ReflectiveImageRegionIterator;
  using Superclass = ImageIteratorWithIndex<TImage>;

  /** Index type alias support While this was already typedef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Index back to itk::Index to that is it not
   * confused with ImageIterator::Index. */
  using IndexType = typename TImage::IndexType;

  /** Image type alias support While this was already typedef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Image back to itk::Image to that is it not
   * confused with ImageIterator::Image. */
  using ImageType = std::remove_const_t<TImage>;

  /** PixelContainer type alias support Used to refer to the container for
   * the pixel data. While this was already typedef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly
   * with gcc. */
  using PixelContainer = typename TImage::PixelContainer;
  using PixelContainerPointer = typename PixelContainer::Pointer;

  /** Region type alias support While this was already typedef'ed in the
   * superclass it needs to be redone here for this subclass to compile
   * properly with gcc.  Note that we have to rescope Region back to
   * itk::ImageRegion so that is it not confused with
   * ImageIterator::Index. */
  using RegionType = typename TImage::RegionType;
  using SizeType = typename TImage::SizeType;
  using SizeValueType = typename TImage::SizeValueType;

  /** Type of the Offset taken from the image.  These type alias are
   * duplicated from the superclass for gcc support. */
  using OffsetType = typename TImage::OffsetType;
  using OffsetValueType = typename OffsetType::OffsetValueType;

  /** Default constructor. */
  ReflectiveImageRegionIterator();

  /** Default destructor.  */
  ~ReflectiveImageRegionIterator() override = default;

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ReflectiveImageRegionIterator(TImage * ptr, const RegionType & region);

  ReflectiveImageRegionIterator(const Self &) = default;

  /** Constructor that can be used to cast from an ImageIterator to an
   * ReflectiveImageRegionIterator. Many routines return an
   * ImageIterator but for a particular task, you may want an
   * ReflectiveImageRegionIterator.  Rather than provide
   * overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from * an
   * ImageIterator to a ReflectiveImageRegionIterator.  */
  template <typename TOtherImage, std::enable_if_t<detail::IsImageIteratorConvertible_v<TOtherImage, TImage>, int> = 0>
  ReflectiveImageRegionIterator(const ImageIteratorWithIndex<TOtherImage> & it)
    : Superclass(it)
  {
    m_BeginOffset.Fill(0);
    m_EndOffset.Fill(0);
  }

  /** Conversion from a writable iterator to a read-only iterator. */
  template <typename TOtherImage, std::enable_if_t<detail::IsImageIteratorConvertible_v<TOtherImage, TImage>, int> = 0>
  ReflectiveImageRegionIterator(const ReflectiveImageRegionIterator<TOtherImage> & it)
    : Superclass(it)
    , m_BeginOffset(it.m_BeginOffset)
    , m_EndOffset(it.m_EndOffset)
  {
    std::copy_n(it.m_IsFirstPass, TImage::ImageDimension, m_IsFirstPass);
  }

  Self &
  operator=(const Self &) = default;

  [[nodiscard]] bool
  IsReflected(unsigned int) const;

  /** Increment (prefix) the fastest moving dimension of the iterator's index.
   * This operator will constrain the iterator within the region (i.e. the
   * iterator will automatically wrap from the end of the row of the region
   * to the beginning of the next row of the region) up until the iterator
   * tries to moves past the last pixel of the region.  Here, the iterator
   * will be set to be one pixel past the end of the region.
   * \sa operator++(int) */
  Self &
  operator++();

  /** Move an iterator to the beginning of the region. */
  void
  GoToBegin();

  /** Is the iterator at the beginning of the region? */
  [[nodiscard]] bool
  IsAtBegin() const
  {
    return !this->m_Remaining;
  }

  /** Set the begin offset.  Forward iteration starts at this offset
   * from the current region.  */
  void
  SetBeginOffset(const OffsetType & offset)
  {
    m_BeginOffset = offset;
  }

  /** Set the end offset.  Reverse iteration starts at this offset
   * from the current region.  */
  void
  SetEndOffset(const OffsetType & offset)
  {
    m_EndOffset = offset;
  }

  /** Get the begin offset.  Forward iteration starts at this offset
   * from the current region.  */
  OffsetType
  GetBeginOffset(const OffsetType &)
  {
    return m_BeginOffset;
  }

  /** Get the end offset.  Reverse iteration starts at this offset
   * from the current region.  */
  OffsetType
  GetEndOffset(const OffsetType &)
  {
    return m_EndOffset;
  }

  /** Fill both offsets with a single value.  */
  void
  FillOffsets(const OffsetValueType & value);

private:
  bool       m_IsFirstPass[TImage::ImageDimension]{};
  OffsetType m_BeginOffset{};
  OffsetType m_EndOffset{};
};

/** \class ReflectiveImageRegionConstIterator
 * \brief Read-only ReflectiveImageRegionIterator: ReflectiveImageRegionConstIterator<TImage> behaves as
 * ReflectiveImageRegionIterator<const TImage>.
 *
 * It is a derived class rather than an alias template so that it supports class template argument deduction.
 *
 * \ingroup ImageIterators
 * \ingroup ITKDistanceMap
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ReflectiveImageRegionConstIterator
  : public ReflectiveImageRegionIterator<std::add_const_t<TImage>>
{
public:
  /** Standard class type alias. */
  using Self = ReflectiveImageRegionConstIterator;
  using Superclass = ReflectiveImageRegionIterator<std::add_const_t<TImage>>;

  using Superclass::Superclass;

  ReflectiveImageRegionConstIterator() = default;

  /** Conversion from ReflectiveImageRegionIterator<const TImage>. */
  ReflectiveImageRegionConstIterator(const Superclass & it)
    : Superclass(it)
  {}
};

// Deduction guide for class template argument deduction (CTAD).
template <typename TImage>
ReflectiveImageRegionConstIterator(const TImage *, const typename TImage::RegionType &)
  -> ReflectiveImageRegionConstIterator<TImage>;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkReflectiveImageRegionIterator.hxx"
#endif

#endif
