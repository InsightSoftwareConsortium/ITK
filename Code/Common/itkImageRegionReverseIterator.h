/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegionReverseIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageRegionReverseIterator_h
#define __itkImageRegionReverseIterator_h

#include "itkImageRegionReverseConstIterator.h"
#include "itkImageConstIterator.h"

namespace itk
{

/** \class ImageRegionReverseIterator
 * \brief Multi-dimensional image iterator which only walks a region.
 * 
 * ImageRegionReverseIterator is a templated class to represent a multi-dimensional
 * iterator. ImageRegionReverseIterator is templated over the image type
 * ImageRegionReverseIterator is constrained to walk only within the 
 * specified region and along a line parallel to one of the coordinate axis.
 *
 * Most of the functionality is inherited from the ImageRegionReverseConstIterator.
 * The current class only adds write access to image pixels.
 *
 * 
 * \sa ImageRegionReverseConstIterator
 *
 * \ingroup ImageIterators
 *
 *
 */
template<typename TImage>
class ImageRegionReverseIterator : public ImageRegionReverseConstIterator<TImage>
{
public:
  /** Standard class typedefs. */
  typedef ImageRegionReverseIterator Self;
  typedef ImageRegionReverseConstIterator<TImage>  Superclass;
  
   /** Types inherited from the Superclass */
  typedef typename Superclass::IndexType              IndexType;
  typedef typename Superclass::IndexValueType         IndexValueType;
  typedef typename Superclass::SizeType               SizeType;
  typedef typename Superclass::SizeValueType          SizeValueType;
  typedef typename Superclass::OffsetType             OffsetType;
  typedef typename Superclass::OffsetValueType        OffsetValueType;
  typedef typename Superclass::RegionType             RegionType;
  typedef typename Superclass::ImageType              ImageType;
  typedef typename Superclass::PixelContainer         PixelContainer;
  typedef typename Superclass::PixelContainerPointer  PixelContainerPointer;
  typedef typename Superclass::InternalPixelType      InternalPixelType;
  typedef typename Superclass::PixelType              PixelType;
  typedef typename Superclass::AccessorType           AccessorType;


  /** Default constructor. Needed since we provide a cast constructor. */
  ImageRegionReverseIterator();
  
  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageRegionReverseIterator(ImageType *ptr, const RegionType& region);

  /** Constructor that can be used to cast from an ImageConstIterator to an
   * ImageRegionReverseIterator. Many routines return an ImageConstIterator but for a
   * particular task, you may want an ImageRegionReverseIterator.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageConstIterators and uses constructors to cast from an
   * ImageConstIterator to a ImageRegionReverseIterator. */
  ImageRegionReverseIterator( const ImageConstIterator<TImage> &it);
  
  /** Set the pixel value */
  void Set( const PixelType & value) const  
    { m_PixelAccessor.Set(*(const_cast<InternalPixelType *>(m_Position)),value); }

  /** Return a reference to the pixel 
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  PixelType & Value(void) 
    { return *(const_cast<InternalPixelType *>(m_Position)); }
  
  /** Return an iterator for the beginning of the region. "Begin"
   * is defined as the first pixel in the region.
   * \deprecated Use GoToBegin() instead */
  Self Begin(void) const;

   /** Return an iterator for the end of the region. "End" is defined
   * as one pixel past the last pixel of the region. 
   * \deprecated Use GoToEnd() instead */
  Self End(void) const;


protected:
  /** the construction from a const iterator is declared protected
      in order to enforce const correctness. */
  ImageRegionReverseIterator( const ImageRegionReverseConstIterator<TImage> &it);
  Self & operator=(const ImageRegionReverseConstIterator<TImage> & it);
 

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRegionReverseIterator.txx"
#endif

#endif 



