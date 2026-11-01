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
#ifndef itkLineIterator_h
#define itkLineIterator_h

#include "itkLineConstIterator.h"

namespace itk
{
/**
 * \class LineIterator
 * \brief An iterator that walks a Bresenham line through an ND image
 *        with write access to pixels.
 *
 * LineIterator is an iterator that walks a Bresenham line
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
 * \sa LineConstIterator
 * \ingroup ITKCommon
 *
 * \sphinx
 * \sphinxexample{Core/Common/IterateLineThroughImage,Iterate Line Through Image}
 * \endsphinx
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT LineIterator : public LineConstIterator<TImage>
{
public:
  /** Standard class type aliases. */
  using Self = LineIterator;

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

  /** \see LightObject::GetNameOfClass() */
  itkVirtualGetNameOfClassMacro(LineConstIterator);

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


  /** Dimension of the image that the iterator walks.  This constant is needed so
   * that functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  static constexpr unsigned int ImageIteratorDimension = TImage::ImageDimension;

  /** Define the superclass */
  using Superclass = LineConstIterator<TImage>;

  /** Inherit types from the superclass */
  using typename Superclass::IndexType;
  using typename Superclass::OffsetType;
  using typename Superclass::SizeType;
  using typename Superclass::RegionType;
  using typename Superclass::ImageType;
  using typename Superclass::PixelContainer;
  using typename Superclass::PixelContainerPointer;
  using typename Superclass::InternalPixelType;
  using typename Superclass::PixelType;
  using typename Superclass::AccessorType;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(LineIterator);

  /** Set the pixel value */
  void
  Set(const PixelType & value)
  {
    // Normally, this would just be the following:
    //   m_Image->SetPixel(m_CurrentImageIndex,value);
    // However, we don't want a warning about m_Image being a ConstPointer
    // in the Superclass.
    const_cast<ImageType *>(this->m_Image.GetPointer())->SetPixel(this->m_CurrentImageIndex, value);
  }

  /** Return a reference to the pixel.
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  const PixelType &
  Value()
  {
    return this->m_Image->GetPixel(this->m_CurrentImageIndex);
  }

  /** operator= is provided to make sure the handle to the image is properly
   * reference counted. */
  Self &
  operator=(const Self & it);

  /** Constructor establishes an iterator to walk along a path */
  LineIterator(ImageType * imagePtr, const IndexType & firstIndex, const IndexType & lastIndex);

  /** Default Destructor. */
  ~LineIterator() override = default;
};
  /** operator= is provided to make sure the handle to the image is properly
   * reference counted. */
  Self &
  operator=(const Self & it);

  /** Constructor establishes an iterator to walk along a line */
  LineConstIterator(const ImageType * imagePtr, const IndexType & firstIndex, const IndexType & lastIndex);

  /** Default Destructor. */
  virtual ~LineConstIterator() = default;

protected: // made protected so other iterators can access
  /** Smart pointer to the source image. */
  typename ImageType::ConstWeakPointer m_Image{};

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
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLineConstIterator.hxx"
#endif

#endif
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
template <typename TImage>
class ITK_TEMPLATE_EXPORT LineConstIterator
{
public:
  /** Standard class type aliases. */
  using Self = LineConstIterator;
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLineIterator.hxx"
#endif

#endif
