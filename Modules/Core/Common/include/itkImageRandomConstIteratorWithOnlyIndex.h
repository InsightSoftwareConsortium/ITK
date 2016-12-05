/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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
#ifndef itkImageRandomConstIteratorWithOnlyIndex_h
#define itkImageRandomConstIteratorWithOnlyIndex_h

#include "itkImageConstIteratorWithOnlyIndex.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"

namespace itk
{
/** \class ImageRandomConstIteratorWithOnlyIndex
 * \brief A multi-dimensional image iterator that visits a random set of locations
 * within an image region, providing index information.
 *
 * ImageRandomConstIteratorWithOnlyIndex is a multi-dimensional iterator class that
 * is templated over image type.  ImageRandomConstIteratorWithOnlyIndex is
 * constrained to walk  only within the specified region. It samples random
 * region positions at each increment or decrement.
 *
 * No access to image data is possible, and thus the class can be used with
 * images of type ImageBase.
 *
 * ImageRandomConstIteratorWithOnlyIndex assumes a particular layout of the image data. The
 * is arranged in a 1D array as if it were [][][][slice][row][col] with
 * Index[0] = col, Index[1] = row, Index[2] = slice, etc.
 *
 * The operator++ method provides a simple syntax for walking around a region
 * of a multidimensional image. operator++ performs a jump to a random position
 * within the specified image region.  This is designed to facilitate the
 * extraction of random indecies from the image.
 *
 * This is the typical use of this iterator in a loop:
 *
 * \code
 *
 * ImageRandomConstIteratorWithOnlyIndex<ImageType> it( image, image->GetRequestedRegion() );
 *
 * it.SetNumberOfSamples(200);
 * it.GoToBegin();
 * while( !it.IsAtEnd() )
 * {
 *   std::cout << it.GetIndex() << std::endl;
 *   ++it;  // here it jumps to another random position inside the region
 *  }
 *
 *  \endcode
 *
 * or
 *
 * \code
 *
 * ImageRandomConstIteratorWithOnlyIndex<ImageType> it( image, image->GetRequestedRegion() );
 *
 * it.SetNumberOfSamples(200);
 * it.GoToEnd();
 * while( !it.IsAtBegin() )
 * {
 *   std::cout << it.GetIndex() << std::endl;
 *   --it;  // here it jumps to another random position inside the region
 *  }
 *
 *  \endcode
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
 * Index-only iterators:
 *
 * \sa ImageConstIteratorWithOnlyIndex
 * \sa ConstNeighborhoodIteratorWithOnlyIndex
 *
 * Pixel data-access iterators:
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
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT ImageRandomConstIteratorWithOnlyIndex:public ImageConstIteratorWithOnlyIndex< TImage >
{
public:
  /** Standard class typedefs. */
  typedef ImageRandomConstIteratorWithOnlyIndex     Self;
  typedef ImageConstIteratorWithOnlyIndex< TImage > Superclass;

  /** Inherit types from the superclass */
  typedef typename Superclass::IndexType             IndexType;
  typedef typename Superclass::SizeType              SizeType;
  typedef typename Superclass::OffsetType            OffsetType;
  typedef typename Superclass::RegionType            RegionType;
  typedef typename Superclass::ImageType             ImageType;
  typedef typename Superclass::IndexValueType        IndexValueType;
  typedef typename Superclass::OffsetValueType       OffsetValueType;
  typedef typename Superclass::SizeValueType         SizeValueType;

  /** Default constructor. Needed since we provide a cast constructor. */
  ImageRandomConstIteratorWithOnlyIndex();
  ~ImageRandomConstIteratorWithOnlyIndex() {}

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageRandomConstIteratorWithOnlyIndex(const ImageType *ptr, const RegionType & region);

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageRandomConstIteratorWithOnlyIndex. Many routines return an ImageIterator, but for a
   * particular task, you may want an ImageRandomConstIteratorWithOnlyIndex.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageRandomConstIteratorWithOnlyIndex. */
  ImageRandomConstIteratorWithOnlyIndex(const ImageConstIteratorWithOnlyIndex< TImage > & it)
  {
    this->ImageConstIteratorWithOnlyIndex< TImage >::operator=(it);
  }

  /** Move an iterator to the beginning of the region. */
  void GoToBegin(void)
  {
    this->RandomJump();
    m_NumberOfSamplesDone = 0L;
  }

  /** Move an iterator to one position past the End of the region. */
  void GoToEnd(void)
  {
    this->RandomJump();
    m_NumberOfSamplesDone = m_NumberOfSamplesRequested;
  }

  /** Is the iterator at the beginning of the region? */
  bool IsAtBegin(void) const
  {
    return ( m_NumberOfSamplesDone == 0L );
  }

  /** Is the iterator at the end of the region? */
  bool IsAtEnd(void) const
  {
    return ( m_NumberOfSamplesDone >= m_NumberOfSamplesRequested );
  }

  /** Increment (prefix) the selected dimension.
   * No bounds checking is performed. \sa GetIndex \sa operator-- */
  Self & operator++()
  {
    this->RandomJump();
    m_NumberOfSamplesDone++;
    return *this;
  }

  /** Decrement (prefix) the selected dimension.
   * No bounds checking is performed. \sa GetIndex \sa operator++ */
  Self & operator--()
  {
    this->RandomJump();
    m_NumberOfSamplesDone--;
    return *this;
  }

  /** Set/Get number of random samples to get from the image region */
  void SetNumberOfSamples(SizeValueType number);

  SizeValueType GetNumberOfSamples() const;

  /** Reinitialize the seed of the random number generator  */
  void ReinitializeSeed();

  void ReinitializeSeed(int);

private:
  void RandomJump();

  typedef Statistics::MersenneTwisterRandomVariateGenerator::Pointer GeneratorPointer;
  GeneratorPointer m_Generator;
  SizeValueType    m_NumberOfSamplesRequested;
  SizeValueType    m_NumberOfSamplesDone;
  SizeValueType    m_NumberOfPixelsInRegion;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRandomConstIteratorWithOnlyIndex.hxx"
#endif

#endif
