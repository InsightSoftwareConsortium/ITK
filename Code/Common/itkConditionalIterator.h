/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkConditionalIterator.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkConditionalIterator_h
#define __itkConditionalIterator_h

#include "itkIndex.h"
#include "itkConditionalConstIterator.h"

namespace itk
{

/** \class ConditionalIterator
 * \brief ConditionalIterator is a base class for other iterators where
 * membership in the set of output pixels is "conditional" upon some
 * property, calculation, etc. For example, a threshold iterator might
 * walk a region and return only those pixels which meet a minimum
 * intensity condition.
 *
 * \ingroup ImageIterators
 */
template<class TImage>
class ITK_EXPORT ConditionalIterator : public ConditionalConstIterator<TImage>
{
public:
  /** Standard class typedefs. */
  typedef ConditionalIterator Self;

  /** Dimension of the image the iterator walks.  This constant is needed so
   * functions that are templated over image iterator type (as opposed to
   * being templated over pixel type and dimension) can have compile time
   * access to the dimension of the image that the iterator walks. */
  itkStaticConstMacro(NDimensions, unsigned int, TImage::ImageDimension);

  /** Index typedef support. */
  typedef typename TImage::IndexType  IndexType;

  /** Size typedef support. */
  typedef typename TImage::SizeType    SizeType;

  /** Region typedef support. */
  typedef typename TImage::RegionType    RegionType;

  /** Image typedef support. */
  typedef TImage   ImageType;

  /** Internal Pixel Type */
  typedef typename TImage::InternalPixelType   InternalPixelType;

  /** External Pixel Type */
  typedef typename TImage::PixelType   PixelType;
  
  /** operator= is provided to make sure the handle to the image is properly
   * reference counted. */
  Self &operator=(const Self& it)
  {
    m_Image = it.m_Image;     // copy the smart pointer
    m_Region = it.m_Region;   // copy the region
  }
  
  /** Constructor */
  ConditionalIterator();

  /** Destructor */
  virtual ~ConditionalIterator();
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkConditionalIterator.txx"
#endif

#endif 
