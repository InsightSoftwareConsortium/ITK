/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageIteratorWithIndex.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageIteratorWithIndex_h
#define __itkImageIteratorWithIndex_h

#include "itkImageConstIteratorWithIndex.h"

namespace itk
{

/**
 * \class ImageIteratorWithIndex
 * \brief Multi-dimensional image iterator.
 * 
 * ImageIteratorWithIndex is a templated class to represent a
 * multi-dimensional iterator. ImageIteratorWithIndex is templated
 * over the dimension of the image and the data type of the image.
 *
 * Most of the ImageIteratorWithIndex functionality is inherited from 
 * its superclass the ImageConstIteratorWithIndex. This iterator
 * only adds read/write access to image pixels.
 *
 * \ingroup ImageIterators
 *
 * \sa ImageConstIteratorWithIndex
 *
 *  */
template<typename TImage>
class ImageIteratorWithIndex : public ImageConstIteratorWithIndex<TImage> {
public:
  /** Standard class typedefs. */
  typedef ImageIteratorWithIndex Self;

  /** Dimension of the image the iterator walks.  This constant is needed so
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  itkStaticConstMacro(ImageIteratorDimension, unsigned int,
                      TImage::ImageDimension);

  /** Define the superclass */
  typedef ImageConstIteratorWithIndex<TImage> Superclass;

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
  ImageIteratorWithIndex();

  /** Default Destructor */
  virtual ~ImageIteratorWithIndex() {};


  /** Copy Constructor. The copy constructor is provided to make sure the
   * handle to the image is properly reference counted. */
  ImageIteratorWithIndex(const Self& it);

  /** Constructor establishes an iterator to walk a particular image and a
   * particular region of that image. */
  ImageIteratorWithIndex(TImage *ptr, const RegionType& region);

  /** operator= is provided to make sure the handle to the image is properly
   * reference counted. */
  Self &operator=(const Self& it);
  
  /** Set the pixel value */
  void Set( const PixelType & value) const  
    { m_PixelAccessor.Set(*(const_cast<InternalPixelType *>(m_Position)),value); }

  /** Return a reference to the pixel 
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  PixelType & Value(void) 
    { return *(const_cast<InternalPixelType *>(m_Position)); }
 
protected:

  /** This constructor is declared protected in order to enforce const-correctness */
  ImageIteratorWithIndex(const ImageConstIteratorWithIndex<TImage> & it);
  Self &operator=(const ImageConstIteratorWithIndex<TImage> & it);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkImageIteratorWithIndex.txx"
#endif

#endif 
