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
#ifndef itkImageRandomNonRepeatingConstIteratorWithIndex_h
#define itkImageRandomNonRepeatingConstIteratorWithIndex_h

#include "itkImageConstIteratorWithIndex.h"
#include <algorithm>
#include <iostream>
#include "itkMersenneTwisterRandomVariateGenerator.h"

namespace itk
{
/** \class NodeOfPermutation
 *  \brief A node to be used when computing permutations.
 *
 * The itk::ImageRandomNonRepeatingIterator works by creating a random
 * permutation of the image pixels and then using that to control the
 * order in which it accesses them.  The classes NodeOfPermutation and
 * RandomPermutation are used to support that.  RandomPermutation is
 * basically container which holds NodeOfPermutation objects.  The
 * node class overloads the < operator, which allows the sort algorithm
 * from the STL to be used on it.
 * \ingroup ITKCommon
 */
class NodeOfPermutation
{
public:
  SizeValueType m_Priority;
  SizeValueType m_Index;
  double        m_Value;

  NodeOfPermutation()
  {
    m_Priority = 0;
    m_Index = 0;
    m_Value = 0.0;
  }

  bool
  operator<(const NodeOfPermutation & b) const
  {
    if (m_Priority == b.m_Priority)
    {
      return m_Value < b.m_Value;
    }
    else
    {
      return m_Priority < b.m_Priority;
    }
  }
};

/** \class RandomPermutation
 * \brief Produce a random permutation of a collection.
 * \ingroup ITKCommon
 */
class RandomPermutation
{
public:
  using GeneratorPointer = typename Statistics::MersenneTwisterRandomVariateGenerator::Pointer;
  NodeOfPermutation * m_Permutation;
  GeneratorPointer    m_Generator;
  SizeValueType       m_Size;

  RandomPermutation(SizeValueType sz)
  {
    m_Size = sz;
    m_Permutation = new NodeOfPermutation[m_Size];
    m_Generator = Statistics::MersenneTwisterRandomVariateGenerator::New();
    this->Shuffle();
  }

  RandomPermutation &
  operator=(const RandomPermutation & it)
  {
    delete[] m_Permutation;
    m_Size = it.m_Size;
    m_Permutation = new NodeOfPermutation[m_Size];
    m_Generator = it.m_Generator;
    return *this;
  }

  void
  SetPriority(SizeValueType i, SizeValueType priority) const
  {
    if (i > m_Size)
    {
      std::ostringstream ostrm;
      ostrm << "Error: RandomPermuation does not have " << i << " elements" << std::endl;
      throw std::runtime_error(ostrm.str());
    }
    else
    {
      m_Permutation[i].m_Priority = priority;
    }
  }

  void
  Shuffle() const
  {
    for (SizeValueType i = 0; i < m_Size; i++)
    {
      m_Permutation[i].m_Value = m_Generator->GetVariateWithClosedRange(1.0);
      m_Permutation[i].m_Index = i;
    }
    std::sort(m_Permutation, m_Permutation + m_Size);
  }

  SizeValueType operator[](SizeValueType i) const { return m_Permutation[i].m_Index; }

  ~RandomPermutation() { delete[] m_Permutation; }

  /** Reinitialize the seed of the random number generator */
  void
  ReinitializeSeed() const
  {
    m_Generator->Initialize();
  }

  void
  ReinitializeSeed(unsigned int seed) const
  {
    m_Generator->SetSeed(seed);
  }
};

/** \class ImageRandomNonRepeatingConstIteratorWithIndex
 * \brief A multi-dimensional image iterator that visits a random set of pixels
 * within an image region.  All pixels in the image will be visited before any
 * are repeated.  A priority image may be passed to the iterator which
 * will cause it to select certain sets of pixels (those with lower priority
 * values) before others.
 *
 *  This class was contributed by Rupert Brooks, McGill Centre for Intelligent
 *  Machines, Montreal, Canada.  It is heavily based on the
 *  ImageRandomIterator class.
 *
 * ImageRandomNonRepeatingConstIteratorWithIndex is a multi-dimensional
 * iterator class that
 * is templated over image type.  ImageRandomNonRepeatingConstIteratorWithIndex
 * is constrained to walk only within the specified region. When first
 * instantiated, it creates (and stores) a random permutation of the image
 * pixels.  It then visits each pixel in the order specified by the
 * permutation.  Thus, iterator++ followed by iterator-- will end up leaving
 * the iterator pointing at the same pixel.  Furthermore, iterating from
 * beginning to end will cover each pixel in the region exactly once.
 *
 * This iterator can be passed an image the same size as the region, which
 * specifies a priority for the pixels.  Within areas of this priority image
 * that have the same value, the pixel selection will be random.  Otherwise
 * the pixel selection will be in the order of the priority image.  In the
 * extreme, this allows the order of the pixel selection to be completely
 * specified.
 *
 * ImageRandomNonRepeatingConstIteratorWithIndex assumes a particular layout
 * of the image data. The is arranged in a 1D array as if it were
 * [][][][slice][row][col] with
 * Index[0] = col, Index[1] = row, Index[2] = slice, etc.
 *
 * \par MORE INFORMATION
 * For a complete description of the ITK Image Iterators and their API, please
 * see the Iterators chapter in the ITK Software Guide.  The ITK Software Guide
 * is available in print and as a free .pdf download from https://www.itk.org.
 *
 * \author Rupert Brooks, McGill Centre for Intelligent Machines. Canada
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
 * \sa ImageRandomNonRepeatingConstIteratorWithIndex  \sa ImageRandomIteratorWithIndex
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
 * \sphinxexample{Core/Common/RandomSelectPixelFromRegionWithoutReplacee,Random Select Pixel From Region Without
 * Replacing} \sphinxexample{Core/Common/PermuteSequenceOfIndices,Permute Sequence Of Indices} \endsphinx
 */
template <typename TImage>
class ITK_TEMPLATE_EXPORT ImageRandomNonRepeatingConstIteratorWithIndex : public ImageConstIteratorWithIndex<TImage>
{
public:
  /** Standard class type aliases. */
  using Self = ImageRandomNonRepeatingConstIteratorWithIndex;
  using Superclass = ImageConstIteratorWithIndex<TImage>;

  /** Inherit types from the superclass */
  using IndexType = typename Superclass::IndexType;
  using SizeType = typename Superclass::SizeType;
  using OffsetType = typename Superclass::OffsetType;
  using RegionType = typename Superclass::RegionType;
  using ImageType = typename Superclass::ImageType;
  using PixelContainer = typename Superclass::PixelContainer;
  using PixelContainerPointer = typename Superclass::PixelContainerPointer;
  using InternalPixelType = typename Superclass::InternalPixelType;
  using PixelType = typename Superclass::PixelType;
  using AccessorType = typename Superclass::AccessorType;
  using IndexValueType = typename Superclass::IndexValueType;
  using OffsetValueType = typename Superclass::OffsetValueType;
  using SizeValueType = typename Superclass::SizeValueType;

  /** Default constructor. Needed since we provide a cast constructor. */
  ImageRandomNonRepeatingConstIteratorWithIndex();
  ~ImageRandomNonRepeatingConstIteratorWithIndex() override { delete m_Permutation; }

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageRandomNonRepeatingConstIteratorWithIndex(const ImageType * ptr, const RegionType & region);

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageRandomNonRepeatingConstIteratorWithIndex. Many routines return an ImageIterator but for a
   * particular task, you may want an ImageRandomNonRepeatingConstIteratorWithIndex.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageRandomNonRepeatingConstIteratorWithIndex. */
  ImageRandomNonRepeatingConstIteratorWithIndex(const ImageConstIteratorWithIndex<TImage> & it)
  {
    this->ImageConstIteratorWithIndex<TImage>::operator=(it);

    m_Permutation = nullptr;
  }

  /** operator= is provided to deep copy m_Permutation. */
  Self &
  operator=(const Self & it);

  /** Move an iterator to the beginning of the region. */
  void
  GoToBegin()
  {
    m_NumberOfSamplesDone = 0L;
    this->UpdatePosition();
  }

  /** Move an iterator to one position past the End of the region. */
  void
  GoToEnd()
  {
    m_NumberOfSamplesDone = m_NumberOfSamplesRequested;
    this->UpdatePosition();
  }

  /** Is the iterator at the beginning of the region? */
  bool
  IsAtBegin() const
  {
    return (m_NumberOfSamplesDone == 0L);
  }

  /** Is the iterator at the end of the region? */
  bool
  IsAtEnd() const
  {
    return (m_NumberOfSamplesDone >= m_NumberOfSamplesRequested);
  }

  /** The moving image dimension. */
  static constexpr unsigned int ImageDimension = TImage::ImageDimension;

  /** Image with priorities */
  using PriorityImageType = itk::Image<SizeValueType, Self::ImageDimension>;

  /** Set the priority image.  The priority image controls the order
      of the random selection.  Pixels of the same priority will be
      ordered randomly, but pixels of lower priority value will be
      selected first.
   */
  void
  SetPriorityImage(const PriorityImageType * priorityImage);

  /** Increment (prefix) the selected dimension.
   * No bounds checking is performed. \sa GetIndex \sa operator-- */
  Self &
  operator++()
  {
    m_NumberOfSamplesDone++;
    this->UpdatePosition();
    return *this;
  }

  /** Decrement (prefix) the selected dimension.
   * No bounds checking is performed. \sa GetIndex \sa operator++ */
  Self &
  operator--()
  {
    m_NumberOfSamplesDone--;
    this->UpdatePosition();
    return *this;
  }

  /** Set/Get number of random samples to get from the image region */
  void
  SetNumberOfSamples(SizeValueType number);

  SizeValueType
  GetNumberOfSamples() const;

  /** Reinitialize the seed of the random number generator  */
  void
  ReinitializeSeed();

  /** Reinitialize the seed of the random number generator with
   *  a specific value */
  void
  ReinitializeSeed(int);

private:
  void
  UpdatePosition();

  SizeValueType       m_NumberOfSamplesRequested;
  SizeValueType       m_NumberOfSamplesDone;
  SizeValueType       m_NumberOfPixelsInRegion;
  RandomPermutation * m_Permutation;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkImageRandomNonRepeatingConstIteratorWithIndex.hxx"
#endif

#endif
