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
#ifndef itkImageRandomConstIteratorWithIndex_h
#define itkImageRandomConstIteratorWithIndex_h

#include "itkImageConstIteratorWithIndex.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"
#include <type_traits> // For enable_if_t, remove_const_t.

namespace itk
{
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
template <typename TImage, bool VIsConst>
class ITK_TEMPLATE_EXPORT ImageRandomIteratorWithIndexBase : public ImageIteratorWithIndexBase<TImage, VIsConst>
{
public:
  /** Standard class type aliases. */
  using Self = ImageRandomIteratorWithIndexBase;
  using Superclass = ImageIteratorWithIndexBase<TImage, VIsConst>;

  /** Inherit types from the superclass */
  using typename Superclass::IndexType;
  using typename Superclass::SizeType;
  using typename Superclass::OffsetType;
  using typename Superclass::RegionType;
  using typename Superclass::ImageType;
  using typename Superclass::ImagePointer;
  using typename Superclass::PixelContainer;
  using typename Superclass::PixelContainerPointer;
  using typename Superclass::InternalPixelType;
  using typename Superclass::PixelType;
  using typename Superclass::AccessorType;
  using typename Superclass::IndexValueType;
  using typename Superclass::OffsetValueType;
  using typename Superclass::SizeValueType;

  /** Default constructor. */
  ImageRandomIteratorWithIndexBase() = default;

  ~ImageRandomIteratorWithIndexBase() override = default;

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageRandomIteratorWithIndexBase(ImagePointer ptr, const RegionType & region);

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageRandomConstIteratorWithIndex. Many routines return an ImageIterator, but for a
   * particular task, you may want an ImageRandomConstIteratorWithIndex.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageRandomConstIteratorWithIndex. */
  ImageRandomIteratorWithIndexBase(const Superclass & it) { this->Superclass::operator=(it); }

  /** Converting constructor: non-const -> const. */
  template <bool VOtherConst, typename = std::enable_if_t<VIsConst && !VOtherConst>>
  ImageRandomIteratorWithIndexBase(const ImageRandomIteratorWithIndexBase<TImage, VOtherConst> & it)
    : Superclass(static_cast<const ImageIteratorWithIndexBase<TImage, VOtherConst> &>(it))
    , m_Generator(it.m_Generator)
    , m_NumberOfSamplesRequested(it.m_NumberOfSamplesRequested)
    , m_NumberOfSamplesDone(it.m_NumberOfSamplesDone)
    , m_NumberOfPixelsInRegion(it.m_NumberOfPixelsInRegion)
  {}

  template <typename, bool>
  friend class ImageRandomIteratorWithIndexBase;

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

template <typename TImage>
class ITK_TEMPLATE_EXPORT ImageRandomConstIteratorWithIndex
  : public ImageRandomIteratorWithIndexBase<TImage, /*VIsConst=*/true>
{
public:
  using Superclass = ImageRandomIteratorWithIndexBase<TImage, /*VIsConst=*/true>;
  using Superclass::Superclass;
};

template <typename TImage>
ImageRandomConstIteratorWithIndex(SmartPointer<TImage>, const typename TImage::RegionType &)
  -> ImageRandomConstIteratorWithIndex<std::remove_const_t<TImage>>;

template <typename TImage>
ImageRandomConstIteratorWithIndex(TImage *, const typename TImage::RegionType &)
  -> ImageRandomConstIteratorWithIndex<TImage>;

template <typename TImage>
ImageRandomConstIteratorWithIndex(const TImage *, const typename TImage::RegionType &)
  -> ImageRandomConstIteratorWithIndex<TImage>;

// Deduction guide for class template argument deduction (CTAD).
template <typename TImage, bool VIsConst = std::is_const_v<TImage>>
ImageRandomIteratorWithIndexBase(SmartPointer<TImage>, const typename TImage::RegionType &)
  -> ImageRandomIteratorWithIndexBase<std::remove_const_t<TImage>, VIsConst>;

template <typename TImage>
ImageRandomIteratorWithIndexBase(TImage *, const typename TImage::RegionType &)
  -> ImageRandomIteratorWithIndexBase<TImage, /*VIsConst=*/false>;

template <typename TImage>
ImageRandomIteratorWithIndexBase(const TImage *, const typename TImage::RegionType &)
  -> ImageRandomIteratorWithIndexBase<TImage, /*VIsConst=*/true>;

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageRandomConstIteratorWithIndex.hxx"
#endif

#endif
