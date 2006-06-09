/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageConstIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageConstIterator_h
#define __itkImageConstIterator_h

#include "itkImage.h"
#include "itkIndex.h"
#include "itkSize.h"
#include "itkOffset.h"

namespace itk
{

/** \class ImageConstIterator
 * \brief A multi-dimensional image iterator templated over image type.
 * 
 * ImageConstIterator is a templated class to represent a multi-dimensional
 * iterator. ImageConstIterator is templated over the type of
 * the image to be iterated over.
 *
 * ImageConstIterator is a base class for all the image iterators. It provides
 * the basic construction and comparison operations.  However, it does not
 * provide mechanisms for moving the iterator.  A subclass of ImageConstIterator
 * must be used to move the iterator.
 * 
 * ImageConstIterator is a multi-dimensional iterator, requiring more information
 * be specified before the iterator can be used than conventional iterators.
 * Whereas the std::vector::iterator from the STL only needs to be passed
 * a pointer to establish the iterator, the multi-dimensional image iterator
 * needs a pointer, the size of the buffer, the size of the region, the
 * start index of the buffer, and the start index of the region. To gain
 * access to this information, ImageConstIterator holds a reference to the image
 * over which it is traversing.
 *
 * ImageConstIterator assumes a particular layout of the image data. In particular,
 * the data is arranged in a 1D array as if it were [][][][slice][row][col]
 * with Index[0] = col, Index[1] = row, Index[2] = slice, etc.
 *
 * \ingroup ImageConstIterators
 * \par MORE INFORMATION
 * For a complete description of the ITK Image Iterators and their API, please
 * see the Iterators chapter in the ITK Software Guide.  The ITK Software Guide
 * is available in print and as a free .pdf download from http://www.itk.org.
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
 */
template<typename TImage>
class ITK_EXPORT ImageConstIterator
{
public:
  /** Standard class typedefs. */
  typedef ImageConstIterator Self;
  
  /** Dimension of the image the iterator walks.  This constant is needed so 
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  itkStaticConstMacro(ImageIteratorDimension, unsigned int,
                      TImage::ImageDimension);

  /** Index typedef support. */
  typedef typename TImage::IndexType  IndexType;
  typedef typename TImage::IndexValueType  IndexValueType;
  
  /** Size typedef support. */
  typedef typename TImage::SizeType    SizeType;
  typedef typename TImage::SizeValueType  SizeValueType;
    
  /** Offset typedef support. */
  typedef typename TImage::OffsetType    OffsetType;
  typedef typename TImage::OffsetValueType  OffsetValueType;
    
  /** Region typedef support. */
  typedef typename TImage::RegionType   RegionType;

  /** Image typedef support. */
  typedef TImage   ImageType;

  /** PixelContainer typedef support. Used to refer to the container for
   * the pixel data. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc. */
  typedef typename TImage::PixelContainer PixelContainer;
  typedef typename PixelContainer::Pointer PixelContainerPointer;
  
  /** Internal Pixel Type */
  typedef typename TImage::InternalPixelType   InternalPixelType;

  /** External Pixel Type */
  typedef typename TImage::PixelType   PixelType;

  /**  Accessor type that convert data between internal and external
   *  representations. */
  typedef typename TImage::AccessorType            AccessorType;
  typedef typename TImage::AccessorFunctorType     AccessorFunctorType;

  /** Default Constructor. Need to provide a default constructor since we
   * provide a copy constructor. */
  ImageConstIterator()
    : m_Region(),
      m_PixelAccessor(),
      m_PixelAccessorFunctor()
  {
    m_Image = 0;
    m_Buffer = 0;
    m_Offset = 0;
    m_BeginOffset = 0;
    m_EndOffset = 0;
    m_PixelAccessorFunctor.SetBegin( m_Buffer );
  }

  /** Default Destructor. */
  virtual ~ImageConstIterator() {};

  /** Copy Constructor. The copy constructor is provided to make sure the
   * handle to the image is properly reference counted. */
  ImageConstIterator(const Self& it)
  {
    m_Image = it.m_Image;     // copy the smart pointer

    m_Region = it.m_Region;
    
    m_Buffer = it.m_Buffer;
    m_Offset = it.m_Offset;
    m_BeginOffset = it.m_BeginOffset;
    m_EndOffset = it.m_EndOffset;
    m_PixelAccessor = it.m_PixelAccessor;
    m_PixelAccessorFunctor = it.m_PixelAccessorFunctor;
    m_PixelAccessorFunctor.SetBegin( m_Buffer );
  }

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageConstIterator( const ImageType *ptr,
                      const RegionType &region )
  {
    m_Image = ptr;
    m_Buffer = m_Image->GetBufferPointer();
    m_Region = region;

    // Compute the start offset
    m_Offset = m_Image->ComputeOffset( m_Region.GetIndex() );
    m_BeginOffset = m_Offset;
    
    // Compute the end offset. If any component of m_Region.GetSize()
    // is zero, the region is not valid and we set the EndOffset
    // to be same as BeginOffset so that iterator end condition is met
    // immediately.
    if (m_Region.GetNumberOfPixels() == 0)
      {
      // region is empty, probably has a size of 0 along one dimension
      m_EndOffset = m_BeginOffset;
      }
    else
      {
      IndexType ind(m_Region.GetIndex());
      SizeType size(m_Region.GetSize());
      for (unsigned int i=0; i < ImageIteratorDimension; ++i)
        {
        ind[i] += (static_cast<IndexValueType>(size[i]) - 1);
        }
      m_EndOffset = m_Image->ComputeOffset( ind );
      m_EndOffset++;
      }

    m_PixelAccessor = ptr->GetPixelAccessor();
    m_PixelAccessorFunctor.SetPixelAccessor( m_PixelAccessor );
    m_PixelAccessorFunctor.SetBegin( m_Buffer );
  }
  
  /** operator= is provided to make sure the handle to the image is properly
   * reference counted. */
  Self &operator=(const Self& it)
  {
    m_Image = it.m_Image;     // copy the smart pointer
    m_Region = it.m_Region;
    
    m_Buffer = it.m_Buffer;
    m_Offset = it.m_Offset;
    m_BeginOffset = it.m_BeginOffset;
    m_EndOffset = it.m_EndOffset;
    m_PixelAccessor = it.m_PixelAccessor;
    m_PixelAccessorFunctor = it.m_PixelAccessorFunctor;
    m_PixelAccessorFunctor.SetBegin( m_Buffer );

    return *this;
  }
  
  /** Get the dimension (size) of the index. */
  static unsigned int GetImageIteratorDimension() 
    {return ImageIteratorDimension;}

  /** Comparison operator. Two iterators are the same if they "point to" the
   * same memory location */
  bool
  operator!=(const Self &it) const
    {
    // two iterators are the same if they "point to" the same memory location
    return (m_Buffer + m_Offset) != (it.m_Buffer + it.m_Offset);
    };

  /** Comparison operator. Two iterators are the same if they "point to" the
   * same memory location */
  bool
  operator==(const Self &it) const
    {
    // two iterators are the same if they "point to" the same memory location
    return (m_Buffer + m_Offset) == (it.m_Buffer + it.m_Offset);
    };
  
  /** Comparison operator. An iterator is "less than" another if it "points to"
   * a lower memory location. */
  bool
  operator<=(const Self &it) const
    {
    // an iterator is "less than" another if it "points to" a lower
    // memory location
    return (m_Buffer + m_Offset) <= (it.m_Buffer + it.m_Offset);
    };

  /** Comparison operator. An iterator is "less than" another if it "points to"
   * a lower memory location. */
  bool
  operator<(const Self &it) const
    {
    // an iterator is "less than" another if it "points to" a lower
    // memory location
    return (m_Buffer + m_Offset) < (it.m_Buffer + it.m_Offset);
    };

  /** Comparison operator. An iterator is "greater than" another if it
   * "points to" a higher location. */
  bool
  operator>=(const Self &it) const
    {
    // an iterator is "greater than" another if it "points to" a higher
    // memory location
    return (m_Buffer + m_Offset) >= (it.m_Buffer + it.m_Offset);
    };

  /** Comparison operator. An iterator is "greater than" another if it
   * "points to" a higher location. */
  bool
  operator>(const Self &it) const
    {
    // an iterator is "greater than" another if it "points to" a higher
    // memory location
    return (m_Buffer + m_Offset) > (it.m_Buffer + it.m_Offset);
    };

  /** Get the index. This provides a read only reference to the index.
   * This causes the index to be calculated from pointer arithmetic and is
   * therefore an expensive operation.
   * \sa SetIndex */
  const IndexType GetIndex() const
    { return m_Image->ComputeIndex( static_cast<OffsetValueType>(m_Offset) );  }

  /** Set the index. No bounds checking is performed.
   * \sa GetIndex */
  virtual void SetIndex(const IndexType &ind)
    { m_Offset = m_Image->ComputeOffset( ind ); }

  /** Get the region that this iterator walks. ImageConstIterators know the
   * beginning and the end of the region of the image to iterate over. */
  const RegionType& GetRegion() const
    { return m_Region; };

  /** Get the image that this iterator walks. */
  const ImageType * GetImage() const
    { return m_Image.GetPointer(); };

  /** Get the pixel value */
  PixelType Get(void) const  
    { return m_PixelAccessorFunctor.Get(*(m_Buffer+m_Offset)); }
  
  /** Return a const reference to the pixel 
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  const PixelType & Value(void) const  
    { return *(m_Buffer + m_Offset); }
 
  /** Return an iterator for the beginning of the region. "Begin"
   * is defined as the first pixel in the region.
   * \deprecated Use GoToBegin() instead.
   */
  Self Begin(void) const;

 /** Move an iterator to the beginning of the region. "Begin" is
  * defined as the first pixel in the region. */
  void GoToBegin()
    {
    m_Offset = m_BeginOffset;
    };

  /** Return an iterator for the end of the region. "End" is defined
   * as one pixel past the last pixel of the region.
   * \deprecated Use GoToEnd() instead.
   */
  Self End(void) const;

 /** Move an iterator to the end of the region. "End" is defined as
  * one pixel past the last pixel of the region. */
  void GoToEnd()
    {
    m_Offset = m_EndOffset;
    };

  /** Is the iterator at the beginning of the region? "Begin" is defined
   * as the first pixel in the region. */
  bool IsAtBegin(void) const
    {
    return (m_Offset == m_BeginOffset);
    }

  /** Is the iterator at the end of the region? "End" is defined as one
   * pixel past the last pixel of the region. */
  bool IsAtEnd(void) const
    {
    return (m_Offset == m_EndOffset);
    }
  

protected: //made protected so other iterators can access 
  typename TImage::ConstWeakPointer  m_Image;
  RegionType                     m_Region;      // region to iterate over
  
  unsigned long  m_Offset;
  unsigned long  m_BeginOffset; // offset to first pixel in region
  unsigned long  m_EndOffset;  // offset to one pixel past last pixel in region

  const InternalPixelType      * m_Buffer;

  AccessorType                   m_PixelAccessor;
  AccessorFunctorType            m_PixelAccessorFunctor;
};

} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_ImageConstIterator(_, EXPORT, x, y) namespace itk { \
  _(1(class EXPORT ImageConstIterator< ITK_TEMPLATE_1 x >)) \
  namespace Templates { typedef ImageConstIterator< ITK_TEMPLATE_1 x > ImageConstIterator##y; } \
  }


#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkImageConstIterator+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkImageConstIterator.txx"
#endif

// #ifndef ITK_MANUAL_INSTANTIATION
// #include "itkImageConstIterator.txx"
// #endif

#endif 
