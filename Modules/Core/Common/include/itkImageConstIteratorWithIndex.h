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
#ifndef itkImageConstIteratorWithIndex_h
#define itkImageConstIteratorWithIndex_h

#include "itkIndex.h"
#include "itkImage.h"
#include <memory>

namespace itk
{
/** \class ImageConstIteratorWithIndex
 * \brief A base class for multi-dimensional iterators templated over image
 * type that are designed to efficiently keep track of the iterator
 * position.
 *
 * ImageConstIteratorWithIndex is a templated class to represent a
 * multi-dimensional iterator, and is templated over both the dimension
 * and data type of the image.
 *
 * ImageConstIteratorWithIndex is a base class for the "WithIndex" family of
 * ITK image iterators, which are designed to efficiently keep track of the
 * image index position of the iterator during increment and decrement
 * operations.  This iterator is a base class and provides only the basic
 * construction and comparison operations. It does not provide mechanisms for
 * moving the iterator.  A subclass of ImageConstIteratorWithIndex must be used
 * to move the iterator.
 *
 * ImageConstIteratorWithIndex is a multi-dimensional iterator, requiring
 * more information be specified before the iterator can be used than
 * conventional iterators.  Whereas the std::vector::iterator from the
 * STL only needs to be passed a pointer to establish the iterator,
 * the multi-dimensional image iterator needs a pointer, the size of
 * the buffer, the size of the region, the start index of the buffer,
 * and the start index of the region. To gain access to this
 * information, ImageConstIteratorWithIndex holds a reference to the image
 * over which it is traversing.
 *
 * ImageConstIteratorWithIndex assumes a particular layout of the image
 * data. In particular, the data is arranged in a 1D array as if it
 * were [][][][slice][row][col] with Index[0] = col, Index[1] = row,
 * Index[2] = slice, etc.
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
 *
 * \ingroup ImageIterators
 * \ingroup ITKCommon
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT ImageConstIteratorWithIndex
{
public:
  /** Standard class typedefs. */
  typedef ImageConstIteratorWithIndex Self;

  /** Dimension of the image that the iterator walks.  This constant is needed so
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  itkStaticConstMacro(ImageDimension, unsigned int, TImage::ImageDimension);

  /** Index typedef support. */
  typedef typename TImage::IndexType         IndexType;
  typedef typename IndexType::IndexValueType IndexValueType;

  /** Size typedef support. */
  typedef typename TImage::SizeType        SizeType;
  typedef typename SizeType::SizeValueType SizeValueType;

  /** Region typedef support. */
  typedef typename TImage::RegionType RegionType;

  /** Image typedef support. */
  typedef TImage ImageType;

  /** PixelContainer typedef support. Used to refer to the container for
   * the pixel data. While this was already typdef'ed in the superclass,
   * it needs to be redone here for this subclass to compile properly with gcc. */
  typedef typename TImage::PixelContainer  PixelContainer;
  typedef typename PixelContainer::Pointer PixelContainerPointer;

  /** Internal Pixel Type */
  typedef typename TImage::InternalPixelType InternalPixelType;

  /** External Pixel Type */
  typedef typename TImage::PixelType PixelType;

  /**  Accessor type that converts data between internal and external
   *  representations. */
  typedef typename TImage::AccessorType        AccessorType;
  typedef typename TImage::AccessorFunctorType AccessorFunctorType;

  /** Type of the Offset taken from the image */
  typedef typename TImage::OffsetType          OffsetType;
  typedef typename OffsetType::OffsetValueType OffsetValueType;

  /** Default Constructor. Need to provide a default constructor since we
   * provide a copy constructor. */
  ImageConstIteratorWithIndex();

  /** Copy Constructor. The copy constructor is provided to make sure the
   * handle to the image is properly reference counted. */
  ImageConstIteratorWithIndex(const Self & it);

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageConstIteratorWithIndex(const TImage *ptr,
                              const RegionType & region);

  /** Default Destructor. */
  virtual ~ImageConstIteratorWithIndex() {};

  /** operator= is provided to make sure the handle to the image is properly
   * reference counted. */
  Self & operator=(const Self & it);

  /** Get the dimension (size) of the index. */
  static unsigned int GetImageDimension()
  {
    return ImageDimension;
  }

  /** Comparison operator. Two iterators are the same if they "point to" the
   * same memory location */
  bool
  operator!=(const Self & it) const
  {
    // two iterators are the same if they "point to" the same memory location
    return ( m_Position ) != ( it.m_Position );
  }

  /** Comparison operator. Two iterators are the same if they "point to" the
   * same memory location */
  bool
  operator==(const Self & it) const
  {
    // two iterators are the same if they "point to" the same memory location
    return ( m_Position ) == ( it.m_Position );
  }

  /** Comparison operator. An iterator is "less than" another if it "points to"
   * a lower memory location. */
  bool
  operator<=(const Self & it) const
  {
    // an iterator is "less than" another if it "points to" a lower
    // memory location
    return ( m_Position ) <= ( it.m_Position );
  }

  /** Comparison operator. An iterator is "less than" another if it "points to"
   * a lower memory location. */
  bool
  operator<(const Self & it) const
  {
    // an iterator is "less than" another if it "points to" a lower
    // memory location
    return ( m_Position ) < ( it.m_Position );
  }

  /** Comparison operator. An iterator is "greater than" another if it
   * "points to" a higher location. */
  bool
  operator>=(const Self & it) const
  {
    // an iterator is "greater than" another if it "points to" a higher
    // memory location
    return ( m_Position ) >= ( it.m_Position );
  }

  /** Comparison operator. An iterator is "greater than" another if it
   * "points to" a higher location. */
  bool
  operator>(const Self & it) const
  {
    // an iterator is "greater than" another if it "points to" a higher
    // memory location
    return ( m_Position ) > ( it.m_Position );
  }

  /** Get the index. This provides a read only reference to the index.
   * \sa SetIndex */
  const IndexType & GetIndex() const
  {
    return m_PositionIndex;
  }

  /** Get the region that this iterator walks. ImageIterators know the
   * beginning and the end of the region of the image to iterate over. */
  const RegionType & GetRegion() const
  {
    return m_Region;
  }

  /** Set the index. No bounds checking is performed.
   * \sa GetIndex */
  void SetIndex(const IndexType & ind)
  {
    m_Position = m_Image->GetBufferPointer() + m_Image->ComputeOffset(ind);
    m_PositionIndex = ind;
  }

  /** Get the pixel value */
  PixelType Get(void) const
  {
    return m_PixelAccessorFunctor.Get(*m_Position);
  }

  /** Return a const reference to the pixel
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  const PixelType & Value(void) const
  {
    return *m_Position;
  }

  /** Move an iterator to the beginning of the region.
   * \deprecated Use GoToBegin() instead */
  itkLegacyMacro(Self Begin(void) const);

  /** Move an iterator to the beginning of the region. */
  void GoToBegin();

  /** Move an iterator to the End of the region.
   * \deprecated Use GoToReverseBegin() instead */
  itkLegacyMacro(Self End(void) const);

  /** Move an iterator to the End of the region. */
  void GoToReverseBegin();

  /** Is the iterator at the beginning of the region? */
  bool IsAtReverseEnd(void) const
  {
    return !m_Remaining;
  }

  /** Is the iterator at the end of the region? */
  bool IsAtEnd(void) const
  {
    return !m_Remaining;
  }

  /** Are there data remaining in the region ? */
  bool Remaining()
  {
    return m_Remaining;
  }

protected: //made protected so other iterators can access
  typename TImage::ConstWeakPointer m_Image;

  IndexType m_PositionIndex;        // Index where we currently are
  IndexType m_BeginIndex;           // Index to start iterating over
  IndexType m_EndIndex;             // Index to finish iterating:
                                    // one pixel past the end of each
                                    // row, col, slice, etc....

  RegionType m_Region;              // region to iterate over

  OffsetValueType m_OffsetTable[ImageDimension + 1];

  const InternalPixelType *m_Position;
  const InternalPixelType *m_Begin;
  const InternalPixelType *m_End;

  bool m_Remaining;

  AccessorType        m_PixelAccessor;
  AccessorFunctorType m_PixelAccessorFunctor;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageConstIteratorWithIndex.hxx"
#endif

#endif
