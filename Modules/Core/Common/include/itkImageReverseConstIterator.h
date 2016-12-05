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
#ifndef itkImageReverseConstIterator_h
#define itkImageReverseConstIterator_h

#include "itkSize.h"
#include "itkImageConstIterator.h"
#include <memory>

namespace itk
{
/** \class ImageReverseConstIterator
 * \brief Multi-dimensional image iterator.
 *
 * ImageReverseConstIterator is a templated class to represent a
 * multi-dimensional iterator. ImageReverseConstIterator is templated over the
 * dimension of the image and the data type of the image.
 *
 * ImageReverseConstIterator is a base class for all the reverse image
 * iterators. It provides the basic construction and comparison
 * operations.  However, it does not provide mechanisms for moving the
 * iterator.  A subclass of ImageReverseConstIterator must be used to move
 * the iterator.
 *
 * ImageReverseConstIterator is a multi-dimensional iterator, requiring
 * more information be specified before the iterator can be used than
 * conventional iterators.  Whereas the std::vector::iterator from the
 * STL only needs to be passed a pointer to establish the iterator,
 * the multi-dimensional image iterator needs a pointer, the size of
 * the buffer, the size of the region, the start index of the buffer,
 * and the start index of the region. To gain access to this
 * information, ImageReverseConstIterator holds a reference to the image
 * over which it is traversing.
 *
 * ImageReverseConstIterator assumes a particular layout of the image
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
 * \sa ImageConstIteratorWithIndex
 * \ingroup ITKCommon
 */
template< typename TImage >
class ITK_TEMPLATE_EXPORT ImageReverseConstIterator
{
public:
  /** Standard class typedefs. */
  typedef ImageReverseConstIterator Self;

  /** Dimension of the image the iterator walks.  This constant is needed so
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  itkStaticConstMacro(ImageIteratorDimension, unsigned int,
                      TImage::ImageDimension);

  /** Run-time type information (and related methods). */
  itkTypeMacroNoParent(ImageReverseConstIterator);

  /** Index typedef support. */
  typedef typename TImage::IndexType      IndexType;

  /** Size typedef support. */
  typedef typename TImage::SizeType      SizeType;

  /** Offset typedef support. */
  typedef typename TImage::OffsetType      OffsetType;

  /** Region typedef support. */
  typedef typename TImage::RegionType RegionType;

  /** Image typedef support. */
  typedef TImage ImageType;

  /** PixelContainer typedef support. Used to refer to the container for
   * the pixel data. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc. */
  typedef typename TImage::PixelContainer  PixelContainer;
  typedef typename PixelContainer::Pointer PixelContainerPointer;

  /** Internal Pixel Type */
  typedef typename TImage::InternalPixelType InternalPixelType;

  /** External Pixel Type */
  typedef typename TImage::PixelType PixelType;

  /**  Accessor type that convert data between internal and external
   *  representations. */
  typedef typename TImage::AccessorType AccessorType;

  /** Functor to choose the appropriate accessor. (for Image vs VectorImage) */
  typedef typename TImage::AccessorFunctorType AccessorFunctorType;

  /** Default Constructor. Need to provide a default constructor since we
   * provide a copy constructor. */
  ImageReverseConstIterator():m_PixelAccessor(), m_PixelAccessorFunctor()
  {
    m_Buffer = 0;
    m_Offset = 0;
    m_BeginOffset = 0;
    m_EndOffset = 0;
    m_PixelAccessorFunctor.SetBegin(m_Buffer);
  }

  /** Default Destructor. */
  virtual ~ImageReverseConstIterator() {}

  /** Copy Constructor. The copy constructor is provided to make sure the
   * handle to the image is properly reference counted. */
  ImageReverseConstIterator(const Self & it)
  {
    m_Image = it.m_Image;     // copy the smart pointer

    m_Region = it.m_Region;

    m_Buffer = it.m_Buffer;
    m_Offset = it.m_Offset;
    m_BeginOffset = it.m_BeginOffset;
    m_EndOffset = it.m_EndOffset;
    m_PixelAccessor = it.m_PixelAccessor;
    m_PixelAccessorFunctor = it.m_PixelAccessorFunctor;
    m_PixelAccessorFunctor.SetBegin(m_Buffer);
  }

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageReverseConstIterator(const ImageType *ptr, const RegionType & region)
  {
    SizeValueType offset;

    m_Image = ptr;
    m_Buffer = m_Image->GetBufferPointer();
    m_Region = region;

    // Compute the end offset, one pixel before the first pixel
    offset = m_Image->ComputeOffset( m_Region.GetIndex() );
    m_EndOffset = offset - 1;

    // Compute the begin offset, the last pixel in the region
    IndexType ind( m_Region.GetIndex() );
    SizeType  size( m_Region.GetSize() );
    for ( unsigned int i = 0; i < TImage::ImageDimension; ++i )
      {
      ind[i] += ( size[i] - 1 );
      }
    m_BeginOffset = m_Image->ComputeOffset(ind);
    m_Offset = m_BeginOffset;

    m_PixelAccessor = ptr->GetPixelAccessor();
    m_PixelAccessorFunctor.SetPixelAccessor(m_PixelAccessor);
    m_PixelAccessorFunctor.SetBegin(m_Buffer);
  }

  /** Constructor that can be used to cast from an ImageConstIterator to an
   * ImageRegionReverseIterator. Many routines return an ImageConstIterator
   * but for a particular task, you may want an
   * ImageRegionReverseIterator.  Rather than provide overloaded APIs
   * that return different types of Iterators, itk returns
   * ImageConstIterators and uses constructors to cast from an
   * ImageConstIterator to a ImageRegionReverseIterator. */
  ImageReverseConstIterator(const ImageConstIterator< TImage > & it)
  {
    m_Image = it.GetImage();
    m_Region = it.GetRegion();
    m_Buffer = m_Image->GetBufferPointer();

    IndexType ind = it.GetIndex();

    m_Offset = m_Image->ComputeOffset(ind);

    // Compute the end offset, one pixel before the first pixel
    m_EndOffset = m_Image->ComputeOffset( m_Region.GetIndex() ) - 1;

    // Compute the begin offset, the last pixel in the region
    IndexType regInd( m_Region.GetIndex() );
    SizeType  regSize( m_Region.GetSize() );
    for ( unsigned int i = 0; i < TImage::ImageDimension; ++i )
      {
      regInd[i] += ( regSize[i] - 1 );
      }
    m_BeginOffset = m_Image->ComputeOffset(regInd);

    m_PixelAccessor = m_Image->GetPixelAccessor();
    m_PixelAccessorFunctor.SetPixelAccessor(m_PixelAccessor);
    m_PixelAccessorFunctor.SetBegin(m_Buffer);
  }

  /** operator= is provided to make sure the handle to the image is properly
   * reference counted. */
  Self & operator=(const Self & it)
  {
    if(this != &it)
      {
      m_Image = it.m_Image;     // copy the smart pointer
      m_Region = it.m_Region;

      m_Buffer = it.m_Buffer;
      m_Offset = it.m_Offset;
      m_BeginOffset = it.m_BeginOffset;
      m_EndOffset = it.m_EndOffset;
      m_PixelAccessor = it.m_PixelAccessor;
      m_PixelAccessorFunctor.SetPixelAccessor(m_PixelAccessor);
      m_PixelAccessorFunctor.SetBegin(m_Buffer);
      }
    return *this;
  }

  /** operator= is provided to make sure the handle to the image is properly
   * reference counted. */
  Self & operator=(const ImageConstIterator< TImage > & it)
  {
    m_Image = it.GetImage();
    m_Region = it.GetRegion();
    m_Buffer = m_Image->GetBufferPointer();

    IndexType ind = it.GetIndex();

    m_Offset = m_Image->ComputeOffset(ind);

    // Compute the end offset, one pixel before the first pixel
    m_EndOffset = m_Image->ComputeOffset( m_Region.GetIndex() ) - 1;

    // Compute the begin offset, the last pixel in the region
    IndexType regInd( m_Region.GetIndex() );
    SizeType  regSize( m_Region.GetSize() );
    for ( unsigned int i = 0; i < TImage::ImageDimension; ++i )
      {
      regInd[i] += ( regSize[i] - 1 );
      }
    m_BeginOffset = m_Image->ComputeOffset(regInd);

    m_PixelAccessor = m_Image->GetPixelAccessor();
    m_PixelAccessorFunctor.SetPixelAccessor(m_PixelAccessor);
    m_PixelAccessorFunctor.SetBegin(m_Buffer);
    return *this;
  }

  /** Get the dimension (size) of the index. */
  static unsigned int GetImageIteratorDimension()
  { return TImage::ImageDimension; }

  /** Comparison operator. Two iterators are the same if they "point to" the
   * same memory location */
  bool
  operator!=(const Self & it) const
  {
    // two iterators are the same if they "point to" the same memory location
    return ( m_Buffer + m_Offset ) != ( it.m_Buffer + it.m_Offset );
  }

  /** Comparison operator. Two iterators are the same if they "point to" the
   * same memory location */
  bool
  operator==(const Self & it) const
  {
    // two iterators are the same if they "point to" the same memory location
    return ( m_Buffer + m_Offset ) == ( it.m_Buffer + it.m_Offset );
  }

  /** Get the index. This provides a read only reference to the index.
   * This causes the index to be calculated from pointer arithmetic and is
   * therefore an expensive operation.
   * \sa SetIndex */
  const IndexType GetIndex()
  { return m_Image->ComputeIndex(m_Offset);  }

  /** Set the index. No bounds checking is performed.
   * \sa GetIndex */
  virtual void SetIndex(const IndexType & ind)
  { m_Offset = m_Image->ComputeOffset(ind); }

  /** Get the region that this iterator walks. ImageReverseConstIterators know the
   * beginning and the end of the region of the image to iterate over. */
  const RegionType & GetRegion() const
  { return m_Region; }

  /** Get the pixel value */
  const PixelType Get(void) const
  { return m_PixelAccessorFunctor.Get( *( m_Buffer + m_Offset ) ); }

  /** Set the pixel value */
  void Set(const PixelType & value) const
  {
    this->m_PixelAccessorFunctor.Set(*( const_cast< InternalPixelType * >(
                                          this->m_Buffer + this->m_Offset ) ), value);
  }

  /** Return a const reference to the pixel
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  const PixelType & Value(void) const
  { return *( m_Buffer + m_Offset ); }

  /** Return a reference to the pixel
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  const PixelType & Value(void)
  { return *( m_Buffer + m_Offset ); }

  /** Return an iterator for the beginning of the region. "Begin" for a reverse
   * iterator is the last pixel in the region.
   * \deprecated Use GoToBegin() instead */
  itkLegacyMacro(Self Begin() const);

  /** Move an iterator to the beginning of the region. "Begin" for a reverse
   * iterator is the last pixel in the region. */
  void GoToBegin()
  {
    m_Offset = m_BeginOffset;
  }

  /** Return an iterator for the end of the region. "End" for a reverse iterator
   * is one pixel before the first pixel in the region.
  * \deprecated Use GoToEnd() instead */
  itkLegacyMacro(Self End() const);

  /** Move an iterator to the end of the region. "End" for a reverse iterator
   * is defined as one pixel before the first pixel in the region. */
  void GoToEnd()
  {
    m_Offset = m_EndOffset;
  }

  /** Is the iterator at the beginning of the (reverse) region? "Begin" for
   * a reverse iterator is the last pixel in the region. */
  bool IsAtBegin()
  {
    return ( m_Offset == m_BeginOffset );
  }

  /** Is the iterator at the end of the (reverse) region? "End" for a reverse
   * iterator is one pixel before the first pixel in the region. */
  bool IsAtEnd()
  {
    return ( m_Offset == m_EndOffset );
  }

protected: //made protected so other iterators can access
  typename ImageType::ConstWeakPointer m_Image;

  RegionType m_Region;                              // region to iterate over

  SizeValueType m_Offset;
  SizeValueType m_BeginOffset;  // offset to last pixel in region
  SizeValueType m_EndOffset;    // offset to one pixel before first pixel

  const InternalPixelType *m_Buffer;

  AccessorType        m_PixelAccessor;
  AccessorFunctorType m_PixelAccessorFunctor;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageReverseConstIterator.hxx"
#endif

#endif
