/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageSliceIteratorWithIndex.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageSliceIteratorWithIndex_h
#define __itkImageSliceIteratorWithIndex_h

#include "itkImageIteratorWithIndex.h"

namespace itk
{

/** \class ImageSliceIteratorWithIndex
 * \brief Multi-dimensional image iterator which walks a region Slice by Slice.
 *
 * This is the typical use of this iterator in a loop:
 *
 * \code
 *  
 * ImageSliceIteratorWithIndex<ImageType> it( image, image->GetRequestedRegion() );
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
 *        value = it.Get();  
 *        it.Set( value * 2 );
 *        ++it;
 *     }
 *     it.NextLine();
 *   }
 *   it.NextSlice();
 *  } 
 *
 *  \endcode
 *
 * \example  Common/itkImageSliceIteratorWithIndex.cxx
 *
 * \ingroup ImageIterators
 */
template<typename TImage>
class ImageSliceIteratorWithIndex : public ImageIteratorWithIndex<TImage>
{
public:
  /** Standard class typedefs. */
  typedef ImageSliceIteratorWithIndex Self;
  typedef ImageIteratorWithIndex<TImage>  Superclass;
  
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
  ImageSliceIteratorWithIndex() : ImageIteratorWithIndex<TImage>() {}
  
  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageSliceIteratorWithIndex( ImageType *ptr,
                      const RegionType & region)
    : ImageIteratorWithIndex<TImage>(ptr, region) 
    {
      m_Direction_A = 0;
      m_Direction_B = 1;
    }

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageSliceIteratorWithIndex. Many routines return an ImageIterator but for a
   * particular task, you may want an ImageSliceIteratorWithIndex.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageSliceIteratorWithIndex. */
  ImageSliceIteratorWithIndex( const ImageIteratorWithIndex<TImage> &it)
    { this->ImageIteratorWithIndex<TImage>::operator=(it); }

  /** Go to the next line.
   * \sa operator++ \sa operator-- \sa PreviousLine \sa EndOfLine \sa End \sa NextSlice */
  void NextLine(void);
  
  /** Go to the next slice.
   * \sa operator++ \sa operator-- \sa PreviousSlice \sa EndOfLine \sa End */
  void NextSlice(void);

  /** Go to the previous line.
   * \sa operator++ \sa operator-- \sa NextLine \sa EndOfLine \sa End \sa NextSlice */
  void PreviousLine(void);
  
  /** Go to the previous slice.
   * \sa operator++ \sa operator-- \sa NextSlice \sa EndOfLine \sa End */
  void PreviousSlice(void);

  /** Test if the index is at the end of line. */
  bool IsAtEndOfLine(void);

  /** Test if the index is at the end of the slice. */
  bool IsAtEndOfSlice(void);

  /** Test if the index is at the begining of line. */
  bool IsAtBeginOfLine(void);

  /** Test if the index is at the begining of the slice. */
  bool IsAtBeginOfSlice(void);

  /** Set the fastest direction of movement. */
  void SetFirstDirection(unsigned int direction);

  /** Set the second fastest direction of movement. */
  void SetSecondDirection(unsigned int direction);

  /** Increment (prefix) the selected dimension.
   * No bounds checking is performed. 
   * \sa GetIndex \sa operator-- */
  Self & operator++();

  /** Decrement (prefix) the selected dimension.
   * No bounds checking is performed. 
   * \sa GetIndex \sa operator++ */
  Self & operator--();

private:
  unsigned long  m_PixelJump;
  unsigned long  m_LineJump;
  unsigned int   m_Direction_A;
  unsigned int   m_Direction_B;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageSliceIteratorWithIndex.txx"
#endif

#endif 
