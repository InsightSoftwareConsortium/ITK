/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSliceConstIteratorWithIndex.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageSliceConstIteratorWithIndex_h
#define __itkImageSliceConstIteratorWithIndex_h

#include "itkImageConstIteratorWithIndex.h"

namespace itk
{

/** \class ImageSliceConstIteratorWithIndex
 * \brief Multi-dimensional image iterator which only walks a region.
 *
 * \brief A multi-dimensional image iterator that extends the
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
 * \example  Common/itkImageSliceIterator.cxx
  *
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
 * \sa ImageConstIteratorWithIndex */
template<typename TImage>
class ITK_EXPORT ImageSliceConstIteratorWithIndex : public ImageConstIteratorWithIndex<TImage>
{
public:
  /** Standard class typedefs. */
  typedef ImageSliceConstIteratorWithIndex Self;
  typedef ImageConstIteratorWithIndex<TImage>  Superclass;
  
  /** Index typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Index back to itk::Index to that is it not
   * confused with ImageIterator::Index. */
  typedef typename TImage::IndexType IndexType;

  /** Image typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Index back to itk::Index to that is it not
   * confused with ImageIterator::Index. */
  typedef TImage ImageType;

  /** Region typedef support. */
  typedef typename TImage::RegionType   RegionType;

  /** PixelContainer typedef support. Used to refer to the container for
   * the pixel data. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc. */
  typedef typename TImage::PixelContainer PixelContainer;
  typedef typename PixelContainer::Pointer PixelContainerPointer;
  
  /** Default constructor. Needed since we provide a cast constructor. */
  ImageSliceConstIteratorWithIndex() : ImageConstIteratorWithIndex<TImage>() {}
  
  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageSliceConstIteratorWithIndex( const ImageType *ptr,
                      const RegionType & region)
    : ImageConstIteratorWithIndex<TImage>(ptr, region) 
    {
      m_Direction_A = 0;
      m_Direction_B = 1;
    }

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageSliceConstIteratorWithIndex. Many routines return an ImageIterator but for a
   * particular task, you may want an ImageSliceConstIteratorWithIndex.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageSliceConstIteratorWithIndex. */
  ImageSliceConstIteratorWithIndex( const ImageConstIteratorWithIndex<TImage> &it)
    { this->ImageConstIteratorWithIndex<TImage>::operator=(it); }

  /** Go to the next line
   * \sa operator++ \sa EndOfLine \sa End \sa NextSlice */
  void NextLine(void);
  
  /** Go to the next slice
   * \sa operator++ \sa EndOfLine \sa End */
  void NextSlice(void);

  /** Go to the next line
   * \sa operator-- \sa BeginOfLine \sa BeginOfSlice \sa Begin */
  void PreviousLine(void);
  
  /** Go to the next slice
   * \sa operator-- \sa BeginOfLine \sa BeginOfSlice \sa Begin */
  void PreviousSlice(void);

  /** Test if the index is at the end of line */
  bool IsAtEndOfLine(void);

  /** Test if the index is at the end of the slice */
  bool IsAtEndOfSlice(void);

  /** Test if the index is at the begin of line */
  bool IsAtReverseEndOfLine(void);

  /** Test if the index is at the begin of the slice */
  bool IsAtReverseEndOfSlice(void);

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
  unsigned long  m_PixelJump;
  unsigned long  m_LineJump;
  unsigned int   m_Direction_A;
  unsigned int   m_Direction_B;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageSliceConstIteratorWithIndex.txx"
#endif

#endif 
