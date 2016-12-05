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
#ifndef itkImageRegionExclusionConstIteratorWithIndex_h
#define itkImageRegionExclusionConstIteratorWithIndex_h

#include "itkImageRegionConstIteratorWithIndex.h"

namespace itk
{
/** \class ImageRegionExclusionConstIteratorWithIndex
 *
 *  \brief A multi-dimensional image iterator that walks an image region,
 *         excluding a second region that may partially or completely
 *         overlap the first, with read-only access to pixels.
 *
 * ImageRegionExclusionConstIteratorWithIndex is a class templated
 * over the image type that represents a multi-dimensional
 * iterator. The exclusion region is set after construction. By
 * default the exclusion region is empty and the iterator will behave
 * as the itk::ImageRegionConstIteratorWithIndex with a penalty in
 * performance due to internal bounds checking. There are no
 * restrictions on valid exclusion regions.
 *
 * As with other ITK image iterators,
 * ImageRegionExclusionConstIteratorWithIndex requires that more
 * information be specified before the iterator can be used than
 * conventional iterators. Whereas the std::vector::iterator from the
 * STL only needs to be passed a pointer to establish the iterator,
 * the multi-dimensional image iterator needs a pointer, the size of
 * the buffer, the size of the region, the start index of the buffer,
 * and the start index of the region. To gain access to this
 * information, ImageRegionExclusionConstIteratorWithIndex holds a
 * reference to the image over which it is traversing.
 *
 * ImageRegionExclusionConstIteratorWithIndex assumes a particular
 * layout of the image data. The is arranged in a 1D array as if it
 * were [][][][slice][row][col] with Index[0] = col, Index[1] = row,
 * Index[2] = slice, etc.
 *
 * The operator++ method provides a simple syntax for walking around a
 * region of a multidimensional image. operator++ iterates across a
 * row, constraining the movement to within a region of image. When
 * the iterator reaches the boundary of the region along a row, the
 * iterator automatically wraps to the next row, starting at the first
 * pixel in the row that is part of the region. This allows for simple
 * processing loops of the form:
 *
 * \code
 *
 *  IteratorType it( image, image->GetRequestedRegion() );
 *
 *  it.SetExclusionRegion( exclusionRegion );
 *  it.GoToBegin();
 *
 *  while( ! it.IsAtEnd() )
 *  {
 *    it.Set( 100.0 + it.Get() );
 *    ++it;
 *  }
 *
 * \endcode
 *
 *  It also can be used for walking in the reverse direction like
 *
 * \code
 *
 *  IteratorType it( image, image->GetRequestedRegion() );
 *
 *  it.SetExclusionRegion( exclusionRegion );
 *  it.GoToEnd();
 *
 *  while( !it.IsAtBegin() )
 *  {
 *    it.Set( 100.0 );
 *    --it;
 *  }
 *
 * \endcode
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
 *
 * \wiki
 * \wikiexample{Iterators/ImageRegionExclusionConstIteratorWithIndex,Iterator over an image skipping a specified region}
 * \endwiki
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT ImageRegionExclusionConstIteratorWithIndex:
  public ImageRegionConstIteratorWithIndex< TImage >
{
public:
  /** Standard class typedefs. */
  typedef ImageRegionExclusionConstIteratorWithIndex  Self;
  typedef ImageRegionConstIteratorWithIndex< TImage > Superclass;

  /** Types inherited from the Superclass */
  typedef typename Superclass::IndexType             IndexType;
  typedef typename Superclass::SizeType              SizeType;
  typedef typename Superclass::OffsetType            OffsetType;
  typedef typename Superclass::RegionType            RegionType;
  typedef typename Superclass::ImageType             ImageType;
  typedef typename Superclass::PixelContainer        PixelContainer;
  typedef typename Superclass::PixelContainerPointer PixelContainerPointer;
  typedef typename Superclass::InternalPixelType     InternalPixelType;
  typedef typename Superclass::PixelType             PixelType;
  typedef typename Superclass::AccessorType          AccessorType;

  /** Default constructor. Needed since we provide a cast constructor. */
  ImageRegionExclusionConstIteratorWithIndex();

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageRegionExclusionConstIteratorWithIndex(const ImageType *ptr,
                                             const RegionType & region);

  /** Constructor that can be used to cast from an ImageRegionConstIteratorWithIndex
   * to an ImageRegionExclusionConstIteratorWithIndex. Many routines return an
   * ImageIterator, but for a particular task, you may want an
   * ImageRegionExclusionConstIteratorWithIndex. Rather than provide overloaded
   * APIs that return different types of Iterators, itk returns ImageIterators
   * and uses constructors to cast from an ImageIterator to a
   * ImageRegionExclusionConstIteratorWithIndex. */
  ImageRegionExclusionConstIteratorWithIndex(const Superclass & it);

  /** Increment (prefix) the fastest moving dimension of the iterator's index.
   * This operator will constrain the iterator within the region (i.e. the
   * iterator will automatically wrap from the end of the row of the region
   * to the beginning of the next row of the region) up until the iterator
   * tries to moves past the last pixel of the region.  Here, the iterator
   * will be set to be one pixel past the end of the region.
   * \sa operator++(int) */
  Self & operator++();

  /** Decrement (prefix) the fastest moving dimension of the iterator's index.
   * This operator will constrain the iterator within the region (i.e. the
   * iterator will automatically wrap from the beginning of the row of the region
   * to the end of the next row of the region) up until the iterator
   * tries to moves past the first pixel of the region.  Here, the iterator
   * will be set to be one pixel past the beginning of the region.
   * \sa operator--(int) */
  Self & operator--();

  /** Method to define the Exclusion region. The iterator will skip pixels
   * inside this region.
   * \warning The exclusion region must be completly contained inside the
   * normal region used to construct the iterator. A border of at least one
   * pixel should exist between the normal region and the exclusion region.
   */
  void SetExclusionRegion(const RegionType & region);

  /** Set the exclusion region to be inset one pixel in from the
   * region the iterator walks. This configures the iterator to only
   * walk the pixels on the boundary of the region.
   */
  void SetExclusionRegionToInsetRegion();

  /** Move an iterator to the beginning of the non-excluded region. */
  void GoToBegin();

  /** Move an iterator to the End of the region. */
  void GoToReverseBegin();

private:

  RegionType m_ExclusionRegion;

  IndexType m_ExclusionBegin;
  IndexType m_ExclusionEnd;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRegionExclusionConstIteratorWithIndex.hxx"
#endif

#endif
