/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageIterator_h
#define __itkImageIterator_h

#include "itkImageConstIterator.h"

namespace itk
{

/**
 * \class ImageIterator
 * \brief Multi-dimensional image iterator.
 * 
 * ImageIterator is a templated class to represent a
 * multi-dimensional iterator. ImageIterator is templated
 * over the dimension of the image and the data type of the image.
 *
 * Most of the ImageIterator functionality is inherited from 
 * its superclass the ImageConstIterator. This iterator
 * only adds read/write access to image pixels.
 *
 * \ingroup ImageIterators
 *
 * \sa ImageConstIterator
 *
 *  */
template<typename TImage>
class ImageIterator : public ImageConstIterator<TImage> {
public:
  /** Standard class typedefs. */
  typedef ImageIterator Self;

  /** Dimension of the image the iterator walks.  This constant is needed so
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  itkStaticConstMacro(ImageIteratorDimension, unsigned int,
                      TImage::ImageDimension);

  /** Define the superclass */
  typedef ImageConstIterator<TImage> Superclass;

  /** Inherit types from the superclass */
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


  /** Default Constructor. Need to provide a default constructor since we
   * provide a copy constructor. */
  ImageIterator();

  /** Default Destructor */
  ~ImageIterator() {};


  /** Copy Constructor. The copy constructor is provided to make sure the
   * handle to the image is properly reference counted. */
  ImageIterator(const Self& it);

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageIterator(TImage *ptr, const RegionType& region);

  /** operator= is provided to make sure the handle to the image is properly
   * reference counted. */
  Self &operator=(const Self& it);
  
  /** Set the pixel value */
  void Set( const PixelType & value) const  
    { m_PixelAccessor.Set(*(const_cast<InternalPixelType *>(m_Buffer)+m_Offset),value); }

  /** Return a reference to the pixel 
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  PixelType & Value(void) 
    { return *(const_cast<InternalPixelType *>(m_Buffer)+m_Offset); }

  /** Return an iterator for the beginning of the region. "Begin"
   * is defined as the first pixel in the region. */
  Self Begin(void) const;

   /** Return an iterator for the end of the region. "End" is defined
   * as one pixel past the last pixel of the region. */
  Self End(void) const;

  /** Get the image that this iterator walks. */
  ImageType * GetImage() const
    { return const_cast<ImageType *>( m_Image.GetPointer() ); };


protected:

  /** This constructor is declared protected in order to enforce const-correctness */
  ImageIterator(const ImageConstIterator<TImage> & it);
  Self &operator=(const ImageConstIterator<TImage> & it);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageIterator.txx"
#endif

#endif 
