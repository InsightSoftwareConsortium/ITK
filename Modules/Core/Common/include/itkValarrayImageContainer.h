/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkValarrayImageContainer_h
#define itkValarrayImageContainer_h

#include "itkObject.h"
#include "itkObjectFactory.h"

#include <utility>
#include <valarray>

namespace itk
{
/** \class ValarrayImageContainer
 *  \brief Defines a front-end to the std\\::\<valarray\> container that conforms to the
 *         ImageContainerInterface.
 *
 * This is a full-fleged Object, so
 * there is modification time, debug, and reference count information.
 *
 * \tparam TElementIdentifier
 *    An INTEGRAL type for use in indexing the valarray.
 *    It must have a \< operator defined for ordering.
 *
 * \tparam TElement
 *    The element type stored in the container.
 *
 * \ingroup ImageObjects
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template<
  typename TElementIdentifier,
  typename TElement
  >
class ITK_TEMPLATE_EXPORT ValarrayImageContainer:
  public Object,
  private std::valarray< TElement >
{
public:
  /** Standard class typedefs. */
  typedef ValarrayImageContainer     Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Save the template parameters. */
  typedef TElementIdentifier ElementIdentifier;
  typedef TElement           Element;

private:
  /** Quick access to the STL valarray type that was inherited. */
  typedef std::valarray< Element > ValarrayType;

protected:
  /** Provide pass-through constructors corresponding to all the STL
   * valarray constructors.  These are for internal use only since
   * this is also an Object which must be constructed through the
   * "New()" routine. */
  ValarrayImageContainer():
    ValarrayType() {}
  ValarrayImageContainer(unsigned long n):
    ValarrayType(n) {}
  ValarrayImageContainer(unsigned long n, const Element & x):
    ValarrayType(n, x) {}
  ValarrayImageContainer(const Self & r):
    ValarrayType(r) {}

public:
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Standard part of every itk Object. */
  itkTypeMacro(ValarrayImageContainer, Object);

  /** Index operator. This version can be an lvalue. */
  TElement & operator[](const ElementIdentifier id)
  { return this->ValarrayType::operator[](id); }

  /** Index operator. This version can only be an rvalue */
  const TElement & operator[](const ElementIdentifier id) const
  { return this->ValarrayType::operator[](id); }

  /** Return a pointer to the beginning of the buffer.  This is used by
   * the image iterator class. */
  TElement * GetBufferPointer()
  {
    if ( this->Size() > 0 )
      {
      return & ( this->ValarrayType::operator[](0) );
      }
    else
      {
      return ITK_NULLPTR;
      }
  }

  /** Get the number of elements currently stored in the container. */
  unsigned long Size(void) const
  { return static_cast< unsigned long >( this->ValarrayType::size() ); }

  /** Tell the container to allocate enough memory to allow at least
   * as many elements as the size given to be stored.  This is NOT
   * guaranteed to actually allocate any memory, but is useful if the
   * implementation of the container allocates contiguous storage. */
  void Reserve(ElementIdentifier num)
  { this->ValarrayType::resize(num); }

  /** Tell the container to try to minimize its memory usage for storage of
   * the current number of elements.  This is NOT guaranteed to decrease
   * memory usage. */
  void Squeeze(void)
  { this->ValarrayType::resize( this->ValarrayType::size() ); }

  /** Tell the container to release any of its allocated memory. */
  void Initialize(void)
  { this->ValarrayType::resize(0); }

  /** Tell the container to release any of its allocated memory. */
  void Fill(const TElement & value)
  { this->ValarrayType::operator=(value); }

public:
  /** PrintSelf routine. Normally this is a protected internal method. It is
   * made public here so that Image can call this method.  Users should not
   * call this method but should call Print() instead.  */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE
  {
    Object::PrintSelf(os, indent);
    // Print out the pointer to bulk data memory. We use const_cast<> to
    // cast away the constness so we can call GetBufferPointer()
    os << indent << "Pointer: "
       << const_cast< ValarrayImageContainer * >( this )->GetBufferPointer()
       << std::endl;

    os << indent << "Size: " << this->Size() << std::endl;
  }
};
} // end namespace itk

#endif
