/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAutoVectorContainer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkAutoVectorContainer_h
#define __itkAutoVectorContainer_h

#include <vector>

#include "itkObject.h"
#include "itkSmartPointer.h"

/**
 * itkAutoVectorContainer
 *
 * Define a front-end to the STL "vector" container that conforms to the
 * itkIndexedContainer interface.  Unlike itkVectorContainer, this version
 * will automatically expand the vector if an indexing operator is called with
 * an index beyond the length of the vector.
 */
template <
  /**
   * An INTEGRAL type for use in indexing the vector.
   */
  typename TElementIdentifier,
  /**
   * The element type stored in the vector.
   */
  typename TElement
  >
class itkAutoVectorContainer:
  public itkObject,
  public std::vector<TElement>
{
public:
  /**
   * Smart pointer typedef support.
   */
  typedef itkAutoVectorContainer     Self;
  typedef itkSmartPointer<Self>  Pointer;
  
  /**
   * Save the template parameters.
   */
  typedef TElementIdentifier  ElementIdentifier;
  typedef TElement            Element;
  
protected:

  /**
   * Quick access to the STL vector type that was inherited.
   */
  typedef std::vector<Element>  Vector;
  
  /**
   * Provide pass-through constructors corresponding to all the STL
   * vector constructors.  These are for internal use only since this is also
   * an itkObject which must be constructed through the "New()" routine.
   */
  itkAutoVectorContainer():
    Vector() {}
  
  itkAutoVectorContainer(size_type n):
    Vector(n) {}
  
  itkAutoVectorContainer(size_type n, const Element& x):
    Vector(n, x) {}
  
  itkAutoVectorContainer(const Self& r):
    Vector(r) {}
  
  template <typename InputIterator>
  itkAutoVectorContainer(InputIterator first, InputIterator last):
    Vector(first, last) {}

public:
  /**
   * Define types needed for the interface.
   */
  typedef Vector::iterator        Iterator;
  typedef Vector::const_iterator  ConstIterator;
  
  /**
   * Declare the public interface routines.
   */
  static Pointer New(void);
  Element GetElement(ElementIdentifier) const;
  void SetElement(ElementIdentifier, Element);
  bool IndexExists(ElementIdentifier) const;
  bool GetElementIfIndexExists(ElementIdentifier, Element*) const;
  void CreateIndex(ElementIdentifier);
  void DeleteIndex(ElementIdentifier);  
  Iterator      Begin(void);
  ConstIterator Begin(void) const;
  Iterator      End(void);
  ConstIterator End(void) const;  
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAutoVectorContainer.cxx"
#endif

#endif
