/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkValarrayImageContainer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkValarrayImageContainer_h
#define __itkValarrayImageContainer_h

#include "itkObject.h"
#include "itkObjectFactory.h"

#include <utility>
#include <valarray>

namespace itk
{

/** \class ValarrayImageContainer
 * Defines a front-end to the STL "valarray" container that conforms to the
 * ImageContainerInterface.  This is a full-fleged Object, so
 * there is modification time, debug, and reference count information.
 *
 * Template parameters for ValarrayImageContainer:
 *
 * TElementIdentifier =
 *    An INTEGRAL type for use in indexing the valarray.
 *    It must have a < operator defined for ordering.
 *
 * TElement =
 *    The element type stored in the container.
 *
 * \ingroup ImageObjects
 * \ingroup DataRepresentation
 */
template <
  typename TElementIdentifier,
  typename TElement
  >
class ValarrayImageContainer: 
  public Object,
  private std::valarray<TElement>
{
public:
  /** Standard class typedefs. */
  typedef ValarrayImageContainer     Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
    
  /** Save the template parameters. */
  typedef TElementIdentifier  ElementIdentifier;
  typedef TElement            Element;
  
private:
  /** Quick access to the STL valarray type that was inherited. */
  typedef std::valarray<Element>  ValarrayType;
  
protected:
  /** Provide pass-through constructors corresponding to all the STL
   * valarray constructors.  These are for internal use only since
   * this is also an Object which must be constructed through the
   * "New()" routine. */
  ValarrayImageContainer():
    ValarrayType() {}
  ValarrayImageContainer(unsigned long n):
    ValarrayType(n) {}
  ValarrayImageContainer(unsigned long n, const Element& x):
    ValarrayType(n, x) {}
  ValarrayImageContainer(const Self& r):
    ValarrayType(r) {}
  
public:
  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Standard part of every itk Object. */
  itkTypeMacro(ValarrayImageContainer, Object);

  /** Index operator. This version can be an lvalue. */
  TElement & operator[](const ElementIdentifier id)
    { return this->ValarrayType::operator[](id); };

  /** Index operator. This version can only be an rvalue */
  const TElement & operator[](const ElementIdentifier id) const
    { return this->ValarrayType::operator[](id); };

  /** Return a pointer to the beginning of the buffer.  This is used by
   * the image iterator class. */
  TElement *GetBufferPointer()
    { return &(this->ValarrayType::operator[](0)); };
  
  /** Get the number of elements currently stored in the container. */
  unsigned long Size(void) const
    { return static_cast<unsigned long>(this->ValarrayType::size()); };

  /** Tell the container to allocate enough memory to allow at least
   * as many elements as the size given to be stored.  This is NOT
   * guaranteed to actually allocate any memory, but is useful if the
   * implementation of the container allocates contiguous storage. */
  void Reserve(ElementIdentifier num)
    { this->ValarrayType::resize(num); };
  
  /** Tell the container to try to minimize its memory usage for storage of
   * the current number of elements.  This is NOT guaranteed to decrease
   * memory usage. */
  void Squeeze(void)
    { this->ValarrayType::resize( this->ValarrayType::size() ); };

  /** Tell the container to release any of its allocated memory. */
  void Initialize(void)
    { this->ValarrayType::resize( 0 ); };
  
public:
  /** PrintSelf routine. Normally this is a protected internal method. It is
   * made public here so that Image can call this method.  Users should not
   * call this method but should call Print() instead.  */
  virtual void PrintSelf(std::ostream& os, Indent indent) const
  {
    Object::PrintSelf(os, indent);
    // Print out the pointer to bulk data memory. We use const_cast<> to
    // cast away the constness so we can call GetBufferPointer()
    os << indent << "Pointer: "
       << const_cast<ValarrayImageContainer*>(this)->GetBufferPointer()
       << std::endl;
    
    os << indent << "Size: " << this->Size() << std::endl;
  };
  
};

} // end namespace itk
  
#endif
