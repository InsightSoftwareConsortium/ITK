/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorContainer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
/**
 * VectorContainer
 *
 * Define a front-end to the STL "vector" container that conforms to the
 * IndexedContainer interface.  This is a full-fleged itkObject, so
 * there is modification time, debug, and reference count information.
 */
#ifndef __itkVectorContainer_h
#define __itkVectorContainer_h

#include <utility>
#include <vector>

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkSmartPointer.h"
#include "itkSetGet.h"

namespace itk
{

/**
 * Template parameters for VectorContainer:
 *
 * TElementIdentifier =
 *     An INTEGRAL type for use in indexing the vector.
 * TElement =
 *    The element type stored in the container.
 */
  
template <
  typename TElementIdentifier,
  typename TElement
  >
class VectorContainer: 
  public itkObject,
  public std::vector<TElement>
{
public:
  /**
   * Smart pointer typedef support.
   */
  typedef VectorContainer        Self;
  typedef itkSmartPointer<Self>  Pointer;
  
  /**
   * Save the template parameters.
   */
  typedef TElementIdentifier  ElementIdentifier;
  typedef TElement            Element;
  
private:
  /**
   * Quick access to the STL vector type that was inherited.
   */
  typedef std::vector<Element>  Vector;
  
protected:
  /**
   * Provide pass-through constructors corresponding to all the STL
   * vector constructors.  These are for internal use only since this is also
   * an itkObject which must be constructed through the "New()" routine.
   */
  VectorContainer():
    Vector() {}
  
  VectorContainer(size_type n):
    Vector(n) {}
  
  VectorContainer(size_type n, const Element& x):
    Vector(n, x) {}
  
  VectorContainer(const Self& r):
    Vector(r) {}
  
  template <typename InputIterator>
  VectorContainer(InputIterator first, InputIterator last):
    Vector(first, last) {}

public:
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * Define types needed for the interface.
   */
  typedef std::pair< const ElementIdentifier , Element >  ValueType;
  
  /**
   * Create a const iterator which simulates the identifier being in the
   * container with the element.
   */
  class ConstIterator
  {
  public:
    typedef ConstIterator Self;

    ConstIterator(ElementIdentifier p, const Vector::const_iterator& r):
      iter(r), pos(p) {}
    ConstIterator(const Self& r): iter(r.iter), pos(r.pos) {}
    ConstIterator& operator=(const Self& r) { iter = r.iter; }
    const ValueType operator* () const { return ValueType(pos, *iter); }
    ConstIterator& operator++()    { ++iter; ++pos; return *this; }
    ConstIterator  operator++(int) { Self tmp = *this; ++iter; ++pos; return tmp; }
    ConstIterator& operator--()    { --iter; --pos; return *this; }
    ConstIterator  operator--(int) { Self tmp = *this; --iter; --pos; return tmp; }
    bool operator==(const Self& r) const { return (iter == r.iter); }
    bool operator!=(const Self& r) const { return (iter != r.iter); }
    
  private:
    Vector::const_iterator iter;
    ElementIdentifier pos;
  };
  
  /**
   * Declare the public interface routines.
   */
  Element& ElementAt(ElementIdentifier);
  Element& CreateElementAt(ElementIdentifier);
  Element GetElement(ElementIdentifier) const;
  void SetElement(ElementIdentifier, Element);
  void InsertElement(ElementIdentifier, Element);
  bool IndexExists(ElementIdentifier) const;
  bool GetElementIfIndexExists(ElementIdentifier, Element*) const;
  void CreateIndex(ElementIdentifier);
  void DeleteIndex(ElementIdentifier);
  ConstIterator Begin(void) const;
  ConstIterator End(void) const;  
};

} // namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorContainer.cxx"
#endif

#endif
