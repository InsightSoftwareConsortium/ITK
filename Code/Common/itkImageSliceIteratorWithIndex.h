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

#include "itkImageSliceConstIteratorWithIndex.h"
#include "itkImageIteratorWithIndex.h"

namespace itk
{

/** \class ImageSliceIteratorWithIndex
 * \brief Multi-dimensional image iterator which only walks a region.
 * 
 * ImageSliceIteratorWithIndex is a templated class to represent a multi-dimensional
 * iterator. ImageSliceIteratorWithIndex is templated over the image type
 * ImageSliceIteratorWithIndex is constrained to walk only within the 
 * specified region and along a line parallel to one of the coordinate axis.
 *
 * Most of the functionality is inherited from the ImageSliceConstIteratorWithIndex.
 * The current class only adds write access to image pixels.
 *
 * 
 * \sa ImageSliceConstIteratorWithIndex
 *
 * \ingroup ImageIterators
 *
 *
 */
template<typename TImage>
class ImageSliceIteratorWithIndex : public ImageSliceConstIteratorWithIndex<TImage>
{
public:
  /** Standard class typedefs. */
  typedef ImageSliceIteratorWithIndex Self;
  typedef ImageSliceConstIteratorWithIndex<TImage>  Superclass;
  
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
  ImageSliceIteratorWithIndex();
  
  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageSliceIteratorWithIndex(ImageType *ptr, const RegionType& region);

  /** Constructor that can be used to cast from an ImageIterator to an
   * ImageSliceIteratorWithIndex. Many routines return an ImageIterator but for a
   * particular task, you may want an ImageSliceIteratorWithIndex.  Rather than
   * provide overloaded APIs that return different types of Iterators, itk
   * returns ImageIterators and uses constructors to cast from an
   * ImageIterator to a ImageSliceIteratorWithIndex. */
  ImageSliceIteratorWithIndex( const ImageIteratorWithIndex<TImage> &it);
  
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
  ImageSliceIteratorWithIndex( const ImageSliceConstIteratorWithIndex<TImage> &it);
  Self & operator=(const ImageSliceConstIteratorWithIndex<TImage> & it);
 

};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageSliceIteratorWithIndex.txx"
#endif

#endif 



