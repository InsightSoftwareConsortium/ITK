/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageContainerInterface.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkImageContainerInterface_h
#define __itkImageContainerInterface_h

#include "itkObject.h"

namespace itk
{

/** \class ImageContainerInterface
 * This should only be used for reference when writing containers
 * conforming to this interface.  ITK uses generic programming to
 * allow container type substitution, so polymorphism is not needed to
 * use containers through this interface.  This means that a container
 * conforming to this interface need not be derived from it, and that
 * their methods should not be virtual.  However, the container must
 * derive from Object in order to support the reference counting,
 * modification time, and debug information required by this
 * interface.
 *
 * Note that many comments refer to a "default element" or "default element
 * value".  This value is equal to the default constructor of the
 * Element type.  Also note that all non-const methods assume that the
 * container was modified, and update the modification time.
 *
 * Template parameters for ImageContainerInterface:
 *
 * TElementIdentifier =
 *    A type that shall be used to index the container.
 *    It must have a < operator defined for ordering.
 *
 * TElement =
 *    The element type stored in the container.
 * 
 * \ingroup ImageObjects
 */
template <typename TElementIdentifier, typename TElement>
class ITK_EXPORT ImageContainerInterface: public Object
{
public:
  /** Standard class typedefs. */
  typedef ImageContainerInterface      Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Standard part of every itk Object. */
  itkTypeMacro(ImageContainerInterface, Object);

  /** Save the template parameters. */
  typedef TElementIdentifier  ElementIdentifier;
  typedef TElement            Element;
  
  /** Index operator. This version can be an lvalue. */
  virtual TElement & operator[](const ElementIdentifier)=0;

  /** Index operator. This version can only be an rvalue */
  virtual const TElement & operator[](const ElementIdentifier) const =0;

  /** Return a pointer to the beginning of the buffer.  This is used by
   * the image iterator class. */
  virtual TElement *GetBufferPointer()=0;
  
  /** Get the number of elements currently stored in the container. */
  virtual unsigned long Size(void) const =0;

  /** Tell the container to allocate enough memory to allow at least
   * as many elements as the size given to be stored.  This is NOT
   * guaranteed to actually allocate any memory, but is useful if the
   * implementation of the container allocates contiguous storage. */
  virtual void Reserve(ElementIdentifier)=0;
  
  /** Tell the container to try to minimize its memory usage for storage of
   * the current number of elements.  This is NOT guaranteed to decrease
   * memory usage. */
  virtual void Squeeze(void)=0;
  
};

} // end namespace itk
  
#endif
