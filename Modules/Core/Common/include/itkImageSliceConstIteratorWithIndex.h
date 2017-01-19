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
#ifndef itkImageSliceConstIteratorWithIndex_h
#define itkImageSliceConstIteratorWithIndex_h

#include "itkImageConstIteratorWithIndex.h"

namespace itk
{
/** \class ImageSliceConstIteratorWithIndex
 * \brief Multi-dimensional image iterator which only walks a region.
 *
 * A multi-dimensional image iterator that extends the
 * ImageLinearConstIteratorWithIndex from iteration along lines in an image to
 * iteration along both lines and planes (slices) within an image.  A slice is
 * defined as a 2D plane spanned by two vectors pointing along orthogonal
 * coordinate axes. The slice orientation of the iterator is defined by
 * specifying its two spanning axes using the methods:
 * \code
 * SetFirstDirection(n)
 * SetSecondDirection(n)
 * \endcode
 * where n is the number of the axis.
 *
 * Use the following methods to move the iterator between slices:
 * \code
 * NextSlice()
 * PreviousSlice()
 * \endcode
 *
 * To test the position of the iterator with respect to the end or beginning of
 * the slice use the following methods:
 * \code
 * IsAtReverseEndOfSlice()
 * IsAtEndOfSlice()
 * \endcode
 *
 * The following code, for example, illustrates the typical use of this
 * iterator.  For more information please see the Software Guide.
 *
 * \code
 *
 * ImageSliceConstIteratorWithIndex<ImageType> it( image, image->GetRequestedRegion() );
 *
 * it.SetFirstDirection(2);
 * it.SetSecondDirection(0);
 *
 * it.GoToBegin();
 * while( !it.IsAtEnd() )
 * {
 *   while( !it.IsAtEndOfSlice() )
 *   {
 *     while( !it.IsAtEndOfLine() )
 *     {
 *        value = it.Get();  // it.Set() doesn't exist in the Const Iterator
 *        ++it;
 *     }
 *     it.NextLine();
 *   }
 *   it.NextSlice();
 *  }
 *
 *  \endcode
 *
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
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT ImageSliceConstIteratorWithIndex:public ImageConstIteratorWithIndex< TImage >
{
public:
  /** Standard class typedefs. */
  typedef ImageSliceConstIteratorWithIndex      Self;
  typedef ImageConstIteratorWithIndex< TImage > Superclass;

  /** Inherit types from the superclass */
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
  ImageSliceConstIteratorWithIndex():ImageConstIteratorWithIndex< TImage >() {}

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageSliceConstIteratorWithIndex(const ImageType *ptr,
                                   const RegionType & region):
    ImageConstIteratorWithIndex< TImage >(ptr, region),
    m_PixelJump(0),
    m_LineJump(0),
    m_Direction_A(0),
    m_Direction_B(1)
  {
  }

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageSliceConstIteratorWithIndex. Many routines return an ImageIterator, but for a
   * particular task, you may want an ImageSliceConstIteratorWithIndex.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageSliceConstIteratorWithIndex. */
  ImageSliceConstIteratorWithIndex(const ImageConstIteratorWithIndex< TImage > & it)
  { this->ImageConstIteratorWithIndex< TImage >::operator=(it); }

  /** Go to the next line
   * \sa operator++ \sa EndOfLine \sa End \sa NextSlice */
  void NextLine();

  /** Go to the first pixel of the current slice */
  void GoToBeginOfSlice();

  /** Go to the next slice
   * \sa operator++ \sa EndOfLine \sa End */
  void NextSlice();

  /** Go to the next line
   * \sa operator-- \sa BeginOfLine \sa BeginOfSlice \sa Begin */
  void PreviousLine();

  /** Go to the next slice
   * \sa operator-- \sa BeginOfLine \sa BeginOfSlice \sa Begin */
  void PreviousSlice();

  /** Test if the index is at the end of line */
  bool IsAtEndOfLine();

  /** Test if the index is at the end of the slice */
  bool IsAtEndOfSlice();

  /** Test if the index is at the begin of line */
  bool IsAtReverseEndOfLine();

  /** Test if the index is at the begin of the slice */
  bool IsAtReverseEndOfSlice();

  /** Set the fastest direction of movement */
  void SetFirstDirection(unsigned int direction);

  /** Set the second fastest direction of movement */
  void SetSecondDirection(unsigned int direction);

  /** Increment (prefix) the selected dimension.
   * No bounds checking is performed.
   * \sa operator-- \sa GetIndex */
  inline Self & operator++();

  /** Decrement (prefix) the selected dimension.
   * No bounds checking is performed.
   * \sa operator++ \sa GetIndex */
  inline Self & operator--();

private:
  SizeValueType m_PixelJump;
  SizeValueType m_LineJump;
  unsigned int  m_Direction_A;
  unsigned int  m_Direction_B;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageSliceConstIteratorWithIndex.hxx"
#endif

#endif
