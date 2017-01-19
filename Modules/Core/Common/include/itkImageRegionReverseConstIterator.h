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
#ifndef itkImageRegionReverseConstIterator_h
#define itkImageRegionReverseConstIterator_h

#include "itkImageReverseConstIterator.h"
#include "itkImageRegionIterator.h"

namespace itk
{
/**
 * \class ImageRegionReverseConstIterator
 * \brief A multi-dimensional image iterator designed to walk a specified image
 * region in reverse.
 *
 * ImageRegionReverseConstIterator is a templated class to represent a
 * multi-dimensional iterator. ImageRegionReverseConstIterator is templated
 * over the dimension of the image and the data type of the
 * image. ImageRegionReverseConstIterator is constrained to walk only
 * within the specified region.
 *
 * ImageRegionReverseConstIterator is a multi-dimensional iterator,
 * requiring more information be specified before the iterator can be
 * used than conventional iterators. Whereas the std::vector::iterator
 * from the STL only needs to be passed a pointer to establish the
 * iterator, the multi-dimensional image iterator needs a pointer, the
 * size of the buffer, the size of the region, the start index of the
 * buffer, and the start index of the region. To gain access to this
 * information, ImageRegionReverseConstIterator holds a reference to the
 * image over which it is traversing.
 *
 * ImageRegionReverseConstIterator assumes a particular layout of the image
 * data. The is arranged in a 1D array as if it were
 * [][][][slice][row][col] with Index[0] = col, Index[1] = row,
 * Index[2] = slice, etc.
 *
 * operator++ provides a simple syntax for walking around a region of
 * a multidimensional image. For a reverse iterator, operator++ moves
 * backwards through the region. operator++ iterates across a row,
 * constraining the movement to within a region of image. When the
 * iterator reaches the boundary of the region along a row, the
 * iterator automatically wraps to the previous row, starting at the last
 * pixel in the row that is part of the region. This allows for simple
 * processing loops of the form:
 *
 * \code
 *
 *      it.GoToBegin();
 *      for (; !it.IsAtEnd(); ++it)
 *         {
 *         std::cout << it.Value() << std::endl;
 *         }
 *
 *  \endcode
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
class ITK_TEMPLATE_EXPORT ImageRegionReverseConstIterator:public ImageReverseConstIterator< TImage >
{
public:
  /** Standard class typedefs. */
  typedef ImageRegionReverseConstIterator     Self;
  typedef ImageReverseConstIterator< TImage > Superclass;

  /** Index typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc. */
  typedef typename Superclass::IndexType      IndexType;

  /** Size typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc. */
  typedef typename Superclass::SizeType      SizeType;

  /** Offset typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc. */
  typedef typename Superclass::OffsetType      OffsetType;

  /** Region typedef support. */
  typedef typename Superclass::RegionType RegionType;

  /** Image typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc. */
  typedef typename Superclass::ImageType ImageType;

  /** PixelContainer typedef support. Used to refer to the container for
   * the pixel data. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc. */
  typedef typename Superclass::PixelContainer PixelContainer;
  typedef typename PixelContainer::Pointer    PixelContainerPointer;

  /** Internal Pixel Type */
  typedef typename Superclass::InternalPixelType InternalPixelType;

  /** External Pixel Type */
  typedef typename Superclass::PixelType PixelType;

  /**  Accessor type that convert data between internal and external
   *  representations. */
  typedef typename Superclass::AccessorType AccessorType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ImageRegionReverseConstIterator, ImageReverseConstIterator);

  /** Default constructor. Needed since we provide a cast constructor. */
  ImageRegionReverseConstIterator():Superclass()
  {
    m_SpanBeginOffset = 0;
    m_SpanEndOffset = 0;
  }

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageRegionReverseConstIterator(const ImageType *ptr, const RegionType & region):
    Superclass(ptr, region)
  {
    m_SpanBeginOffset = this->m_BeginOffset;
    m_SpanEndOffset = this->m_BeginOffset - static_cast< OffsetValueType >( this->m_Region.GetSize()[0] );
  }

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageRegionReverseConstIterator. Many routines return an ImageIterator
   * but for a particular task, you may want an
   * ImageRegionReverseConstIterator.  Rather than provide overloaded APIs
   * that return different types of Iterators, itk returns
   * ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageRegionReverseConstIterator. */
  ImageRegionReverseConstIterator(const ImageConstIterator< TImage > & it):Superclass(it)
  {
    IndexType ind = this->GetIndex();

    m_SpanBeginOffset = this->m_Offset + static_cast< OffsetValueType >( this->m_Region.GetSize()[0] )
                        - ( ind[0] - this->m_Region.GetIndex()[0] );
    m_SpanEndOffset = m_SpanBeginOffset - static_cast< OffsetValueType >( this->m_Region.GetSize()[0] );
  }

  /** Constructor that takes in a reverse image iterator.  This can be used
   * to cast between the various types of reverse image iterators. */
  ImageRegionReverseConstIterator(const ImageReverseConstIterator< TImage > & it):Superclass(it)
  {
    IndexType ind = this->GetIndex();

    m_SpanBeginOffset = this->m_Offset + static_cast< OffsetValueType >( this->m_Region.GetSize()[0] )
                        - ( ind[0] - this->m_Region.GetIndex()[0] );
    m_SpanEndOffset = m_SpanBeginOffset - static_cast< OffsetValueType >( this->m_Region.GetSize()[0] );
  }

  /** Constructor that takes in an image region iterator.  This can be used
  * to cast between the various types of reverse image iterators. */
  ImageRegionReverseConstIterator(const ImageRegionIterator< TImage > & it):Superclass(it)
  {
    IndexType ind = this->GetIndex();

    m_SpanBeginOffset = this->m_Offset + static_cast< OffsetValueType >( this->m_Region.GetSize()[0] )
                        - ( ind[0] - this->m_Region.GetIndex()[0] );
    m_SpanEndOffset = m_SpanBeginOffset - static_cast< OffsetValueType >( this->m_Region.GetSize()[0] );
  }

  /** Move an iterator to the beginning of the region. "Begin" for a reverse
   * iterator is the last pixel in the region. */
  void GoToBegin()
  {
    Superclass::GoToBegin();

    // reset the span offsets
    m_SpanBeginOffset = this->m_BeginOffset;
    m_SpanEndOffset   = this->m_BeginOffset - static_cast< OffsetValueType >( this->m_Region.GetSize()[0] );
  }

  /** Move an iterator to the end of the region. "End" for a reverse iterator
   * is defined as one pixel before the first pixel in the region. */
  void GoToEnd()
  {
    Superclass::GoToEnd();

    // reset the span offsets
    m_SpanEndOffset = this->m_EndOffset;
    m_SpanBeginOffset = m_SpanEndOffset + static_cast< OffsetValueType >( this->m_Region.GetSize()[0] );
  }

  /** Return an iterator for the beginning of the region. "Begin" for a reverse
   * iterator is the last pixel in the region.
   * \deprecated Use GoToBegin() instead */
  itkLegacyMacro(Self Begin(void) const);

  /** Return an iterator for the end of the region. "End" for a
  * reverse iterator is one pixel before the first pixel in the
  * region.
  * \deprecated Use GoToEnd() instead */
  itkLegacyMacro(Self End(void) const);

  /** Set the index. No bounds checking is performed. This is overridden
   * from the parent because we have an extra ivar.
   * \sa GetIndex */
  void SetIndex(const IndexType & ind) ITK_OVERRIDE
  {
    Superclass::SetIndex(ind);
    m_SpanBeginOffset = this->m_Offset + static_cast< OffsetValueType >( this->m_Region.GetSize()[0] )
                        - ( ind[0] - this->m_Region.GetIndex()[0] );
    m_SpanEndOffset = m_SpanBeginOffset - static_cast< OffsetValueType >( this->m_Region.GetSize()[0] );
  }

  /** iterator's index.  For a reverse iterator, this moves backwards
   * through the region.  This operator will constrain the iterator
   * within the region (i.e. the iterator will automatically wrap from
   * the start of the row of the region to the end of the previous row
   * of the region) up until the iterator tries to moves before the
   * first pixel of the region.  Here, the iterator will be set to be
   * one pixel before the start of the region.
   * \sa operator++(int). */
  Self &
  operator++()
  {
    if ( --this->m_Offset <= m_SpanEndOffset )
      {
      // We have past the beginning of the span (row), need to wrap around.

      // First move forward one pixel, because we are going to use a different
      // algorithm to compute the next pixel
      this->m_Offset++;

      // Get the index of the first pixel on the span (row)
      typename ImageConstIterator< TImage >::IndexType
      ind = this->m_Image->ComputeIndex( static_cast< OffsetValueType >( this->m_Offset ) );

      const typename ImageConstIterator< TImage >::IndexType &
      startIndex = this->m_Region.GetIndex();
      const typename ImageConstIterator< TImage >::SizeType &
      size = this->m_Region.GetSize();

      // Deccrement along a row, then wrap at the beginning of the region row.
      bool         done;
      unsigned int dim;

      // Check to see if we are past the first pixel in the region
      // Note that --ind[0] moves to the previous pixel along the row.
      done = ( --ind[0] == startIndex[0] - 1 );
      for ( unsigned int i = 1; done && i < this->ImageIteratorDimension; i++ )
        {
        done = ( ind[i] == startIndex[i] );
        }

      // if the iterator is outside the region (but not past region begin) then
      // we need to wrap around the region
      dim = 0;
      if ( !done )
        {
        while ( ( dim < this->ImageIteratorDimension - 1 )
                && ( ind[dim] < startIndex[dim] ) )
          {
          ind[dim] = startIndex[dim] + static_cast< OffsetValueType >( size[dim] ) - 1;
          ind[++dim]--;
          }
        }
      this->m_Offset = this->m_Image->ComputeOffset(ind);
      m_SpanBeginOffset = this->m_Offset;
      m_SpanEndOffset = m_SpanBeginOffset - static_cast< OffsetValueType >( size[0] );
      }
    return *this;
  }

  /** Decrement (prefix) the fastest moving dimension of the iterator's index.
   * For a reverse iterator, this moves forward through the region.
   * This operator will constrain the iterator within the region (i.e. the
   * iterator will automatically wrap from the end of the row of the region
   * to the start of the next row of the region) up until the iterator
   * tries to moves past the last pixel of the region.  Here, the iterator
   * will be set to be one pixel past the end of the region.
   * \sa operator--(int) */
  Self & operator--()
  {
    if ( ++this->m_Offset >= m_SpanBeginOffset )
      {
      // We have reached the end of the span (row), need to wrap around.

      // First back up one pixel, because we are going to use a different
      // algorithm to compute the next pixel
      --this->m_Offset;

      // Get the index of the last pixel on the span (row)
      typename ImageConstIterator< TImage >::IndexType
      ind = this->m_Image->ComputeIndex( static_cast< OffsetValueType >( this->m_Offset ) );

      const typename ImageIterator< TImage >::IndexType &
      startIndex = this->m_Region.GetIndex();
      const typename ImageIterator< TImage >::SizeType &
      size = this->m_Region.GetSize();

      // Increment along a row, then wrap at the end of the region row.
      bool         done;
      unsigned int dim;

      // Check to see if we are past the last pixel in the region
      // Note that ++ind[0] moves to the next pixel along the row.
      done = ( ++ind[0] == startIndex[0] + static_cast< OffsetValueType >( size[0] ) );
      for ( unsigned int i = 1; done && i < this->ImageIteratorDimension; i++ )
        {
        done = ( ind[i] == startIndex[i] + static_cast< OffsetValueType >( size[i] ) - 1 );
        }

      // if the iterator is outside the region (but not past region end) then
      // we need to wrap around the region
      dim = 0;
      if ( !done )
        {
        while ( ( dim < this->ImageIteratorDimension - 1 )
                && ( ind[dim] > startIndex[dim] + static_cast< OffsetValueType >( size[dim] ) - 1 ) )
          {
          ind[dim] = startIndex[dim];
          ind[++dim]++;
          }
        }
      this->m_Offset = this->m_Image->ComputeOffset(ind);
      m_SpanBeginOffset = this->m_Offset;
      m_SpanEndOffset = this->m_Offset - static_cast< OffsetValueType >( size[0] );
      }
    return *this;
  }

protected:
  SizeValueType m_SpanBeginOffset; // offset to last pixel in the row
  SizeValueType m_SpanEndOffset;   // offset to one pixel before the row
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRegionReverseConstIterator.hxx"
#endif

#endif
