/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkImageContainerInterface.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef __itkImageContainerInterface_h
#define __itkImageContainerInterface_h

#include "itkObject.h"
#include "itkSmartPointer.h"

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
 */

template <typename TElementIdentifier, typename TElement>
class ImageContainerInterface: public Object
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef ImageContainerInterface      Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /** \typedef
   * Save the template parameters.
   */
  typedef TElementIdentifier  ElementIdentifier;
  typedef TElement            Element;

  /**
   * Index operator. This version can be an lvalue.
   */
  virtual TElement & operator[](const ElementIdentifier)=0;

  /**
   * Index operator. This version can only be an rvalue
   */
  virtual const TElement & operator[](const ElementIdentifier) const =0;

  /**
   * Return a pointer to the beginning of the buffer.  This is used by
   * the image iterator class.
   */
  virtual TElement *GetBufferPointer()=0;
  
  /**
   * Get the number of elements currently stored in the container.
   */
  virtual unsigned long Size(void) const =0;

  /**
   * Tell the container to allocate enough memory to allow at least
   * as many elements as the size given to be stored.  This is NOT
   * guaranteed to actually allocate any memory, but is useful if the
   * implementation of the container allocates contiguous storage.
   */
  virtual void Reserve(ElementIdentifier)=0;
  
  /**
   * Tell the container to try to minimize its memory usage for storage of
   * the current number of elements.  This is NOT guaranteed to decrease
   * memory usage.
   */
  virtual void Squeeze(void)=0;
  
  /**
   * Standard part of every itk Object.
   */
  itkTypeMacro(ImageContainerInterface, Object);
};

} // end namespace itk
  
#endif
