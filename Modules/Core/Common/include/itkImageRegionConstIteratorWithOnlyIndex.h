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
#ifndef itkImageRegionConstIteratorWithOnlyIndex_h
#define itkImageRegionConstIteratorWithOnlyIndex_h

#include "itkImageConstIteratorWithOnlyIndex.h"

namespace itk
{
/** \class ImageRegionConstIteratorWithOnlyIndex
 * \brief A multi-dimensional iterator templated over image type that walks
 * an image region and is specialized to keep track of its index location
 * and safely support images of type ImageBase.
 *
 * The "WithOnlyIndex" family of iteators was designed for algorithms that use only
 * the locations of image pixels in calculations. Unlike
 * ImageRegionIterator, which calculates an index only when requested,
 * ImageRegionIteratorWithOnlyIndex maintains its index location as a member
 * variable that is updated during increment and decrement operations.
 * Iteration speed is penalized, but index queries become more efficient.
 *
 * ImageRegionConstIteratorWithOnlyIndex is a multi-dimensional iterator,
 * requiring more information be specified before the iterator can be
 * used than conventional iterators. Whereas the std::vector::iterator
 * from the STL only needs to be passed a pointer to establish the
 * iterator, the multi-dimensional image iterator needs a pointer,
 * the size of the region, and the start index of the region. To gain access to this
 * information, ImageRegionConstIteratorWithOnlyIndex holds a reference to the
 * image over which it is traversing.
 *
 * ImageRegionConstIteratorWithOnlyIndex assumes a particular layout of
 * the image data. The is arranged in a 1D array as if it were
 * [][][][slice][row][col] with Index[0] = col, Index[1] = row,
 * Index[2] = slice, etc.
 *
 * operator++ provides a simple syntax for walking around a region of
 * a multidimensional image. operator++ iterates across a row, constraining
 * the movement to within a region of image. When the iterator reaches
 * the boundary of the region along a row, the iterator automatically
 * wraps to the next row, starting at the first pixel in the row that is
 * part of the region. This allows for simple processing loops of the form:
 *
 * \code
 *
 *  IteratorType it( image, image->GetRequestedRegion() );
 *
 *  it.Begin();
 *
 *  while( ! it.IsAtEnd() )
 *  {
 *    std::cout << it.GetIndex() << std::endl;
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
 *  it.End();
 *
 *  while( !it.IsAtBegin() )
 *  {
 *    std::cout << it.GetIndex() << std::endl;
 *    --it;
 *  }
 *
 * \endcode
 *
 * \par MORE INFORMATION
 *
 * For a complete description of the ITK Image Iterators and their API, please
 * see the Iterators chapter in the ITK Software Guide.  The ITK Software Guide
 * is available in print and as a free .pdf download from https://www.itk.org.
 *
 * \ingroup ImageIterators
 *
 * Index-only iterators:
 *
 * \sa ImageRandomConstIteratorWithOnlyIndex
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
class ITK_TEMPLATE_EXPORT ImageRegionConstIteratorWithOnlyIndex:public ImageConstIteratorWithOnlyIndex< TImage >
{
public:
  /** Standard class typedefs. */
  typedef ImageRegionConstIteratorWithOnlyIndex     Self;
  typedef ImageConstIteratorWithOnlyIndex< TImage > Superclass;

  /**
   * Index typedef support. While these were already typdef'ed in the superclass
   * they need to be redone here for this subclass to compile properly with gcc.
   */
  /** Types inherited from the Superclass */
  typedef typename Superclass::IndexType             IndexType;
  typedef typename Superclass::SizeType              SizeType;
  typedef typename Superclass::OffsetType            OffsetType;
  typedef typename Superclass::RegionType            RegionType;
  typedef typename Superclass::ImageType             ImageType;

  /** Default constructor. Needed since we provide a cast constructor. */
  ImageRegionConstIteratorWithOnlyIndex():ImageConstIteratorWithOnlyIndex< TImage >() {}

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageRegionConstIteratorWithOnlyIndex(const TImage *ptr, const RegionType & region):
    ImageConstIteratorWithOnlyIndex< TImage >(ptr, region) {}

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageRegionConstIteratorWithOnlyIndex. Many routines return an ImageIterator but for a
   * particular task, you may want an ImageRegionConstIteratorWithOnlyIndex.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageRegionConstIteratorWithOnlyIndex. */
  ImageRegionConstIteratorWithOnlyIndex(const ImageConstIteratorWithOnlyIndex< TImage > & it)
  { this->ImageConstIteratorWithOnlyIndex< TImage >::operator=(it); }

  /** Increment (prefix) the fastest moving dimension of the iterator's index.
   * This operator will constrain the iterator within the region (i.e. the
   * iterator will automatically wrap from the end of the row of the region
   * to the beginning of the next row of the region) up until the iterator
   * tries to moves past the last pixel of the region.  Here, the iterator
   * will be set to be one pixel past the end of the region.
   * \sa operator-- */
  Self & operator++();

  /** Decrement (prefix) the fastest moving dimension of the iterator's index.
   * This operator will constrain the iterator within the region (i.e. the
   * iterator will automatically wrap from the beginning of the row of the
   * region to the end of the previous row of the region) up until the iterator
   * tries to moves past the first pixel of the region.  Here, the iterator
   * will be set to be one pixel past the beginning of the region.
   * \sa operator++ */
  Self & operator--();
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRegionConstIteratorWithOnlyIndex.hxx"
#endif

#endif
