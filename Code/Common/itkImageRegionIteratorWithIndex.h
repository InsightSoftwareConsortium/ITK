/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageRegionIteratorWithIndex.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageRegionIteratorWithIndex_h
#define __itkImageRegionIteratorWithIndex_h

#include "itkImageRegionConstIteratorWithIndex.h"
#include "itkImageIteratorWithIndex.h"

namespace itk
{

/** \class ImageRegionIteratorWithIndex
 * \brief Multi-dimensional image iterator which only walks a region.
 * 
 * ImageRegionIteratorWithIndex is a templated class to represent a
 * multi-dimensional iterator. ImageRegionIteratorWithIndex is templated
 * over the image type.  ImageRegionIteratorWithIndex is constrained to
 * walk only within the specified region.
 *
 * Most of the functionality is inherited from the ImageRegionConstIteratorWithIndex,
 * only write access to image pixels is added by the current class.
 *
 * \sa ImageRegionConstIteratorWithIndex
 *
 * \ingroup ImageIterators
 */
template<typename TImage>
class ITK_EXPORT ImageRegionIteratorWithIndex : public ImageRegionConstIteratorWithIndex<TImage>
{
public:
  /** Standard class typedefs. */
  typedef ImageRegionIteratorWithIndex Self;
  typedef ImageRegionConstIteratorWithIndex<TImage>  Superclass;
  
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
  ImageRegionIteratorWithIndex();
  
  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageRegionIteratorWithIndex(TImage *ptr, const RegionType& region);

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageRegionIteratorWithIndex. Many routines return an ImageIterator but for a
   * particular task, you may want an ImageRegionIteratorWithIndex.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageRegionIteratorWithIndex. */
  ImageRegionIteratorWithIndex( const ImageIteratorWithIndex<TImage> &it);

  /** Set the pixel value */
  void Set( const PixelType & value) const  
    { m_PixelAccessor.Set(*(const_cast<InternalPixelType *>(m_Position)),value); }

  /** Return a reference to the pixel 
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  PixelType & Value(void) 
    { return *(const_cast<InternalPixelType *>(m_Position)); }
 
protected:
  /** the construction from a const iterator is declared protected
      in order to enforce const correctness. */
  ImageRegionIteratorWithIndex( const ImageRegionConstIteratorWithIndex<TImage> &it);
  Self &operator=(const ImageRegionConstIteratorWithIndex<TImage> & it);
  
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageRegionIteratorWithIndex.txx"
#endif

#endif 
