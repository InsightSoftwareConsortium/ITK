/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRandomConstIteratorWithIndex.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageRandomConstIteratorWithIndex_h
#define __itkImageRandomConstIteratorWithIndex_h

#include "itkImageConstIteratorWithIndex.h"

namespace itk
{

/** \class ImageRandomConstIteratorWithIndex
 * \brief Multi-dimensional image iterator which walks randomly within a region.
 * 
 * ImageRandomConstIteratorWithIndex is a templated class to represent a multi-dimensional
 * iterator. ImageRandomConstIteratorWithIndex is templated over the image type
 * ImageRandomConstIteratorWithIndex is constrained to walk only within the 
 * specified region. It samples random pixel positions at each increment.
 *
 * ImageRandomConstIteratorWithIndex is a multi-dimensional iterator, requiring more
 * information be specified before the iterator can be used than conventional
 * iterators. Whereas the std::vector::iterator from the STL only needs to be
 * passed a pointer to establish the iterator, the multi-dimensional image
 * iterator needs a pointer, the size of the buffer, the size of the region,
 * the start index of the buffer, and the start index of the region. To gain
 * access to this information, ImageRandomConstIteratorWithIndex holds a reference to the
 * image over which it is traversing.
 *
 * ImageRandomConstIteratorWithIndex assumes a particular layout of the image data. The
 * is arranged in a 1D array as if it were [][][][slice][row][col] with
 * Index[0] = col, Index[1] = row, Index[2] = slice, etc.
 *
 * operator++ provides a simple syntax for walking around a region of
 * a multidimensional image. operator++ performs a jump to a random
 * position within the specified image region. 
 * This is designed to facilitate the extraction
 * of random samples from the image.
 *
 * This is the typical use of this iterator in a loop:
 *
 * \code
 *  
 * ImageRandomConstIteratorWithIndex<ImageType> it( image, image->GetRequestedRegion() );
 * 
 * it.SetNumberOfSamples(200);
 * it.GoToBegin();
 * while( !it.IsAtEnd() )
 * {
 *   it.Get();
 *   ++it;  // here it jumps to another random position inside the region
 *  } 
 *
 *  \endcode
 *
 * or
 *
 * \code
 *  
 * ImageRandomConstIteratorWithIndex<ImageType> it( image, image->GetRequestedRegion() );
 * 
 * it.SetNumberOfSamples(200);
 * it.GoToEnd();
 * while( !it.IsAtBegin() )
 * {
 *   it.Get();
 *   --it;  // here it jumps to another random position inside the region
 *  } 
 *
 *  \endcode
 *
 * \warning Incrementing the iterator (++it) followed by a decrement (--it)
 * or vice versa does not in general return the iterator to the same position.
 *
 * \example  Common/itkImageRandomConstIteratorWithIndexTest.cxx
 *
 *
 * \ingroup ImageIterators
 *
 *
 */
template<typename TImage>
class ITK_EXPORT ImageRandomConstIteratorWithIndex : public ImageConstIteratorWithIndex<TImage>
{
public:
  /** Standard class typedefs. */
  typedef ImageRandomConstIteratorWithIndex Self;
  typedef ImageConstIteratorWithIndex<TImage>  Superclass;
  
  /** Index typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Index back to itk::Index to that is it not
   * confused with ImageIterator::Index. */
  typedef typename TImage::IndexType   IndexType;

  /** Region typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Region back to itk::ImageRegion so that is
   * it not confused with ImageIterator::Index. */
  typedef typename TImage::RegionType RegionType;
  
  /** Image typedef support. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc.
   * Note that we have to rescope Index back to itk::Index to that is it not
   * confused with ImageIterator::Index. */
  typedef TImage ImageType;

  /** PixelContainer typedef support. Used to refer to the container for
   * the pixel data. While this was already typdef'ed in the superclass
   * it needs to be redone here for this subclass to compile properly with gcc. */
  typedef typename TImage::PixelContainer PixelContainer;
  typedef typename PixelContainer::Pointer PixelContainerPointer;
  
  /** Default constructor. Needed since we provide a cast constructor. */
  ImageRandomConstIteratorWithIndex();
  ~ImageRandomConstIteratorWithIndex() {};
  
  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageRandomConstIteratorWithIndex(const ImageType *ptr, const RegionType& region);

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageRandomConstIteratorWithIndex. Many routines return an ImageIterator but for a
   * particular task, you may want an ImageRandomConstIteratorWithIndex.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageRandomConstIteratorWithIndex. */
  ImageRandomConstIteratorWithIndex( const ImageConstIteratorWithIndex<TImage> &it)
    { this->ImageConstIteratorWithIndex<TImage>::operator=(it); }

  /** Move an iterator to the beginning of the region. */
  void GoToBegin(void);

  /** Move an iterator to the End of the region. */
  void GoToEnd(void);

  /** Is the iterator at the beginning of the region? */
  bool IsAtBegin(void) const
    { return (m_NumberOfSamplesDone > m_NumberOfSamplesRequested) ; }

  /** Is the iterator at the end of the region? */
  bool IsAtEnd(void) const
    { return (m_NumberOfSamplesDone > m_NumberOfSamplesRequested);  }
 
  /** Increment (prefix) the selected dimension.
   * No bounds checking is performed. \sa GetIndex \sa operator-- */
  Self & operator++();

  /** Decrement (prefix) the selected dimension.
   * No bounds checking is performed. \sa GetIndex \sa operator++ */
  Self & operator--();
  
  /** Set/Get number of random samples to get from the image region */
  void SetNumberOfSamples( unsigned long number );
  unsigned long GetNumberOfSamples( void ) const;

  /** Reinitialize the seed of the random number generator  */
  static void ReinitializeSeed();

private:
    unsigned long  m_NumberOfSamplesRequested;
    unsigned long  m_NumberOfSamplesDone;
    unsigned long  m_NumberOfPixelsInRegion;
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRandomConstIteratorWithIndex.txx"
#endif

#endif 



