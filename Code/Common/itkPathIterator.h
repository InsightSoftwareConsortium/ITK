/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkPathIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkPathIterator_h
#define __itkPathIterator_h

#include "itkIndex.h"
#include "itkImage.h"
#include "itkPath.h"
#include "itkPathConstIterator.h"

namespace itk
{

/**
 * \class PathIterator
 * \brief PathIterator iterates (traces) over a path through an image.
 *
 * This iterator visits only those indices of the image which are overlapped by
 * the path.  All indicies are visited in path order.  If a path crosses itself
 * at an index, that index of the image will be visited twice.
 * 
 * \ingroup ImageIterators
 * \ingroup Paths
 *
 */
template<class TImage, class TPath>
class ITK_EXPORT PathIterator : public PathConstIterator<TImage, TPath>
{
public:
  /** Standard class typedefs. */
  typedef PathIterator Self;

  /** Dimension of the image the iterator walks.  This constant is needed so 
   * that functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  itkStaticConstMacro(ImageIteratorDimension, unsigned int,
                      TImage::ImageDimension);

  /** Define the superclass */
  typedef PathConstIterator<TImage,TPath> Superclass;

  /** Inherit types from the superclass */
  typedef typename Superclass::IndexType              IndexType;
  typedef typename Superclass::IndexValueType         IndexValueType;
  typedef typename Superclass::OffsetType             OffsetType;
  typedef typename Superclass::OffsetValueType        OffsetValueType;
  typedef typename Superclass::SizeType               SizeType;
  typedef typename Superclass::SizeValueType          SizeValueType;
  typedef typename Superclass::RegionType             RegionType;
  typedef typename Superclass::ImageType              ImageType;
  typedef typename Superclass::PixelContainer         PixelContainer;
  typedef typename Superclass::PixelContainerPointer  PixelContainerPointer;
  typedef typename Superclass::InternalPixelType      InternalPixelType;
  typedef typename Superclass::PixelType              PixelType;
  typedef typename Superclass::AccessorType           AccessorType;
  typedef typename Superclass::PathType               PathType;
  typedef typename Superclass::PathInputType          PathInputType;
  typedef typename Superclass::PathOutputType         PathOutputType;

  /** Run-time type information (and related methods). */
  itkTypeMacro(PathIterator, PathConstIterator);
  
  
  
  /** Set the pixel value */
  void Set( const PixelType & value)
    {
    // Normally, this would just be the following:
    //   m_Image->SetPixel(m_CurrentImageIndex,value);
    // However, we don't want a warning about m_Image being a ConstPointer
    // in the Superclass.
    const_cast<ImageType *>(m_Image.GetPointer())->
      SetPixel(m_CurrentImageIndex,value);
    }

  /** Return a reference to the pixel 
   * This method will provide the fastest access to pixel
   * data, but it will NOT support ImageAdaptors. */
  PixelType & Value(void) 
    {
    return m_Image->GetPixel(m_CurrentImageIndex);
    }

  /** operator= is provided to make sure the handles to the image and path are
   * properly reference counted. */
  Self &operator=(const Self& it);

  /** Constructor establishes an iterator to walk along a path */
  PathIterator(ImageType *imagePtr, const PathType  *pathPtr);

  /** Default Destructor. */
  virtual ~PathIterator() {};
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkPathIterator.txx"
#endif

#endif 
