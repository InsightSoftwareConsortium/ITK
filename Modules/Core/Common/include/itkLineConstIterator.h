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
#ifndef itkLineConstIterator_h
#define itkLineConstIterator_h

#include "itkIndex.h"
#include "itkImage.h"
#include "itkWeakPointer.h"
#include <type_traits>

namespace itk
{
/**
 * \class LineConstIterator
 * \brief An iterator that walks a Bresenham line through an ND image
 *        with read-only access to pixels.
 *
 * LineConstIterator is an iterator that walks a Bresenham line
 * through an image.  The iterator is constructed similar to other
 * image iterators, except instead of specifying a region to
 * traverse, you specify two indices. The interval specified by
 * the two indices is closed.  So, a line iterator specified with
 * the same start and end index will visit exactly one pixel.
 *
   \code
   LineConstIterator<ImageType> it(image, I1, I2);
   while (!it.IsAtEnd())
   {
      // visits at least 1 pixel
   }
   \endcode
 *
 * \author Benjamin King, Experimentelle Radiologie, Medizinische
 * Hochschule Hannover.
 *
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/IterateLineThroughImageWithoutWriteAccess,Iterate Line Through Image Without Write Access}
 * \endsphinx
 */
template <typename TImage, bool VIsConst>
class ITK_TEMPLATE_EXPORT LineIteratorBase
{
public:
  /** Standard class type aliases. */
  using Self = LineIteratorBase;

  /** Dimension of the image that the iterator walks.  This constant is needed so
   * that functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  static constexpr unsigned int ImageIteratorDimension = TImage::ImageDimension;

  /** Index type alias support */
  using IndexType = typename TImage::IndexType;

  /** Offset type alias support */
  using OffsetType = typename TImage::OffsetType;

  /** Size type alias support */
  using SizeType = typename TImage::SizeType;

  /** Region type alias support */
  using RegionType = typename TImage::RegionType;

  /** Spacing type alias support */
  using SpacingType = typename TImage::SpacingType;

  /** Origin type alias support */
  using PointType = typename TImage::PointType;

  /** Image type alias support */
  using ImageType = TImage;

  /** PixelContainer type alias support Used to refer to the container for
   * the pixel data. While this was already typedef'ed in the superclass,
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

  using ImageWeakPointer = std::conditional_t<VIsConst, typename TImage::ConstWeakPointer, WeakPointer<TImage>>;
  using ImagePointer = std::conditional_t<VIsConst, const TImage *, TImage *>;

  /** \see LightObject::GetNameOfClass() */
  itkVirtualGetNameOfClassMacro(LineIteratorBase);

  /** Get the dimension (size) of the index. */
  static unsigned int
  GetImageIteratorDimension()
  {
    return TImage::ImageDimension;
  }

  /** Get the index. This provides a read only reference to the index. */
  const IndexType
  GetIndex()
  {
    return m_CurrentImageIndex;
  }

  /** Get the pixel value */
  [[nodiscard]] const PixelType
  Get() const
  {
    return m_Image->GetPixel(m_CurrentImageIndex);
  }

  /** Set the pixel value. SFINAE-gated on !VIsConst. */
  template <bool VCopy = VIsConst, std::enable_if_t<!VCopy, int> = 0>
  void
  Set(const PixelType & value)
  {
    m_Image->SetPixel(m_CurrentImageIndex, value);
  }

  /** Is the iterator at the end of the line? */
  [[nodiscard]] bool
  IsAtEnd() const
  {
    return m_IsAtEnd;
  }

  /** Move an iterator to the beginning of the line. */
  void
  GoToBegin();

  /** Walk forward along the line to the next index in the image. */
  void
  operator++();

  /** operator= is provided to make sure the handle to the image is properly
   * reference counted. */
  Self &
  operator=(const Self & it);

  /** Constructor establishes an iterator to walk along a line */
  LineIteratorBase(ImagePointer imagePtr, const IndexType & firstIndex, const IndexType & lastIndex);

  /** Converting constructor: non-const -> const. */
  template <bool VOtherConst, typename = std::enable_if_t<VIsConst && !VOtherConst>>
  LineIteratorBase(const LineIteratorBase<TImage, VOtherConst> & it)
    : m_Image(it.m_Image)
    , m_Region(it.m_Region)
    , m_IsAtEnd(it.m_IsAtEnd)
    , m_CurrentImageIndex(it.m_CurrentImageIndex)
    , m_StartIndex(it.m_StartIndex)
    , m_LastIndex(it.m_LastIndex)
    , m_EndIndex(it.m_EndIndex)
    , m_MainDirection(it.m_MainDirection)
    , m_AccumulateError(it.m_AccumulateError)
    , m_IncrementError(it.m_IncrementError)
    , m_MaximalError(it.m_MaximalError)
    , m_OverflowIncrement(it.m_OverflowIncrement)
    , m_ReduceErrorAfterIncrement(it.m_ReduceErrorAfterIncrement)
  {}

  template <typename, bool>
  friend class LineIteratorBase;

  /** Default Destructor. */
  virtual ~LineIteratorBase() = default;

protected: // made protected so other iterators can access
  LineIteratorBase() = default;

  /** Smart pointer to the source image. */
  ImageWeakPointer m_Image{};

  /** Region type to iterate over. */
  RegionType m_Region{};

  /** Is the iterator at the end of its walk? */
  bool m_IsAtEnd{};

  /** Start, end and current ND index position in the image of the line */
  IndexType m_CurrentImageIndex{};
  IndexType m_StartIndex{};
  IndexType m_LastIndex{};
  IndexType m_EndIndex{}; // one past the end of the line in the m_MainDirection

  /** Variables that drive the Bresenham-Algorithm */
  // The dimension with the largest difference between start and end
  unsigned int m_MainDirection{};

  // Accumulated error for the other dimensions
  IndexType m_AccumulateError{};

  // Increment for the error for each step. Two times the difference between
  // start and end
  IndexType m_IncrementError{};

  // If enough is accumulated for a dimension, the index has to be
  // incremented. Will be the number of pixels in the line
  IndexType m_MaximalError{};

  // Direction of increment. -1 or 1
  IndexType m_OverflowIncrement{};

  // After an overflow, the accumulated error is reduced again. Will be
  // two times the number of pixels in the line
  IndexType m_ReduceErrorAfterIncrement{};
};

template <typename TImage>
class ITK_TEMPLATE_EXPORT LineConstIterator : public LineIteratorBase<TImage, /*VIsConst=*/true>
{
public:
  using Superclass = LineIteratorBase<TImage, /*VIsConst=*/true>;
  using Superclass::Superclass;
};

template <typename TImage>
LineConstIterator(SmartPointer<TImage>, const typename TImage::RegionType &)
  -> LineConstIterator<std::remove_const_t<TImage>>;

template <typename TImage>
LineConstIterator(TImage *, const typename TImage::RegionType &) -> LineConstIterator<TImage>;

template <typename TImage>
LineConstIterator(const TImage *, const typename TImage::RegionType &) -> LineConstIterator<TImage>;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLineConstIterator.hxx"
#endif

#endif
