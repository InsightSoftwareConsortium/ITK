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
#ifndef itkImageRandomIteratorWithIndex_h
#define itkImageRandomIteratorWithIndex_h


#include "itkImageIteratorWithIndex.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include <type_traits> // For remove_const_t.

namespace itk
{
/** \class ImageRandomIteratorWithIndex
 * \brief A multi-dimensional image iterator that visits a random set of pixels
 * within an image region.
 *
 * This class is a subclass of itkImageRandomConstIteratorWithIndex that adds
 * write-access functionality.  Please see itkImageRandomConstIteratorWithIndex
 * for more information.
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
class ITK_TEMPLATE_EXPORT ImageRandomIteratorWithIndex : public ImageIteratorWithIndex<TImage>
{
  template <typename>
  friend class ImageRandomIteratorWithIndex;

public:
  /** Standard class type aliases. */
  using Self = ImageRandomIteratorWithIndex;
  using Superclass = ImageIteratorWithIndex<TImage>;

  /** Inherit types from the superclass */
  using typename Superclass::IndexType;
  using typename Superclass::SizeType;
  using typename Superclass::OffsetType;
  using typename Superclass::RegionType;
  using typename Superclass::ImageType;
  using typename Superclass::PixelContainer;
  using typename Superclass::PixelContainerPointer;
  using typename Superclass::InternalPixelType;
  using typename Superclass::PixelType;
  using typename Superclass::AccessorType;
  using typename Superclass::IndexValueType;
  using typename Superclass::OffsetValueType;
  using typename Superclass::SizeValueType;

  /** Default constructor. */
  ImageRandomIteratorWithIndex() = default;

  ~ImageRandomIteratorWithIndex() override = default;

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageRandomIteratorWithIndex(TImage * ptr, const RegionType & region);

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageRandomIteratorWithIndex. Many routines return an ImageIterator, but for a
   * particular task, you may want an ImageRandomIteratorWithIndex.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageRandomIteratorWithIndex. */
  template <typename TOtherImage, std::enable_if_t<detail::IsImageIteratorConvertible_v<TOtherImage, TImage>, int> = 0>
  ImageRandomIteratorWithIndex(const ImageIteratorWithIndex<TOtherImage> & it)
    : Superclass(it)
  {}

  /** Conversion from a writable iterator to a read-only iterator. */
  template <typename TOtherImage, std::enable_if_t<detail::IsImageIteratorConvertible_v<TOtherImage, TImage>, int> = 0>
  ImageRandomIteratorWithIndex(const ImageRandomIteratorWithIndex<TOtherImage> & it)
    : Superclass(it)
    , m_Generator(it.m_Generator)
    , m_NumberOfSamplesRequested(it.m_NumberOfSamplesRequested)
    , m_NumberOfSamplesDone(it.m_NumberOfSamplesDone)
    , m_NumberOfPixelsInRegion(it.m_NumberOfPixelsInRegion)
  {}

  /** Move an iterator to the beginning of the region. */
  void
  GoToBegin()
  {
    this->RandomJump();
    m_NumberOfSamplesDone = 0L;
  }

  /** Move an iterator to one position past the End of the region. */
  void
  GoToEnd()
  {
    this->RandomJump();
    m_NumberOfSamplesDone = m_NumberOfSamplesRequested;
  }

  /** Is the iterator at the beginning of the region? */
  [[nodiscard]] bool
  IsAtBegin() const
  {
    return m_NumberOfSamplesDone == 0L;
  }

  /** Is the iterator at the end of the region? */
  [[nodiscard]] bool
  IsAtEnd() const
  {
    return m_NumberOfSamplesDone >= m_NumberOfSamplesRequested;
  }

  /** Increment (prefix) the selected dimension.
   * No bounds checking is performed. \sa GetIndex \sa operator-- */
  Self &
  operator++()
  {
    this->RandomJump();
    ++m_NumberOfSamplesDone;
    return *this;
  }

  /** Decrement (prefix) the selected dimension.
   * No bounds checking is performed. \sa GetIndex \sa operator++ */
  Self &
  operator--()
  {
    this->RandomJump();
    --m_NumberOfSamplesDone;
    return *this;
  }

  /** Set/Get number of random samples to extract from the image region. */
  void
  SetNumberOfSamples(SizeValueType number)
  {
    m_NumberOfSamplesRequested = number;
  }

  [[nodiscard]] SizeValueType
  GetNumberOfSamples() const
  {
    return m_NumberOfSamplesRequested;
  }

  /** Reinitialize the seed of the random number generator. */
  void
  ReinitializeSeed();

  void
  ReinitializeSeed(int);

private:
  /** Execute a random jump. */
  void
  RandomJump();

  using GeneratorPointer = Statistics::MersenneTwisterRandomVariateGenerator::Pointer;
  GeneratorPointer m_Generator{ Statistics::MersenneTwisterRandomVariateGenerator::New() };
  SizeValueType    m_NumberOfSamplesRequested{};
  SizeValueType    m_NumberOfSamplesDone{};
  SizeValueType    m_NumberOfPixelsInRegion{};
};

// Deduction guide for class template argument deduction (CTAD).
template <typename TImage>
ImageRandomIteratorWithIndex(SmartPointer<TImage>, const typename TImage::RegionType &)
  -> ImageRandomIteratorWithIndex<TImage>;

/** \class ImageRandomConstIteratorWithIndex
 * \brief A multi-dimensional image iterator that visits a random set of pixels
 * within an image region.
 *
 * ImageRandomConstIteratorWithIndex is a multi-dimensional iterator class that
 * is templated over image type.  ImageRandomConstIteratorWithIndex is
 * constrained to walk  only within the specified region. It samples random
 * pixel positions at each increment or decrement.
 *
 * ImageRandomConstIteratorWithIndex assumes a particular layout of the image data. The
 * is arranged in a 1D array as if it were [][][][slice][row][col] with
 * Index[0] = col, Index[1] = row, Index[2] = slice, etc.
 *
 * The operator++ method provides a simple syntax for walking around a region
 * of a multidimensional image. operator++ performs a jump to a random position
 * within the specified image region.  This is designed to facilitate the
 * extraction of random samples from the image.
 *
 * This is the typical use of this iterator in a loop:
 *
   \code

   ImageRandomConstIteratorWithIndex<ImageType> it( image, image->GetRequestedRegion() );

   it.SetNumberOfSamples(200);
   it.GoToBegin();
   while( !it.IsAtEnd() )
   {
     it.Get();
     ++it;  // here it jumps to another random position inside the region
    }

    \endcode
 *
 * or
 *
   \code

   ImageRandomConstIteratorWithIndex<ImageType> it( image, image->GetRequestedRegion() );

   it.SetNumberOfSamples(200);
   it.GoToEnd();
   while( !it.IsAtBegin() )
   {
     it.Get();
     --it;  // here it jumps to another random position inside the region
    }

    \endcode
 *
 * \warning Incrementing the iterator (++it) followed by a decrement (--it)
 * or vice versa does not in general return the iterator to the same position.
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
 *
 * \sphinx
 * \sphinxexample{Core/Common/RandomSelectOfPixelsFromRegion,Random Selection Of Pixels From Region}
 * \endsphinx
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ImageRandomConstIteratorWithIndex
  : public ImageRandomIteratorWithIndex<std::add_const_t<TImage>>
{
public:
  /** Standard class type alias. */
  using Self = ImageRandomConstIteratorWithIndex;
  using Superclass = ImageRandomIteratorWithIndex<std::add_const_t<TImage>>;

  using Superclass::Superclass;

  ImageRandomConstIteratorWithIndex() = default;

  /** Conversion from ImageRandomIteratorWithIndex<const TImage>. */
  ImageRandomConstIteratorWithIndex(const Superclass & it)
    : Superclass(it)
  {}
};

// Deduction guides for class template argument deduction (CTAD).
template <typename TImage>
ImageRandomConstIteratorWithIndex(const TImage *, const typename TImage::RegionType &)
  -> ImageRandomConstIteratorWithIndex<TImage>;

template <typename TImage>
ImageRandomConstIteratorWithIndex(SmartPointer<TImage>, const typename TImage::RegionType &)
  -> ImageRandomConstIteratorWithIndex<std::remove_const_t<TImage>>;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageRandomIteratorWithIndex.hxx"
#endif

#endif
