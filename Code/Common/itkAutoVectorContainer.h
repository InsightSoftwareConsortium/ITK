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

#include "itkContainerInterfaces.h"
#include <vector>

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
  public itkIndexedContainer< TElementIdentifier , TElement >,
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
   * Declare the public interface routines.
   */
  static Pointer New(void);
  virtual Element& operator[](ElementIdentifier);
  virtual bool IndexExists(ElementIdentifier);
  virtual void CreateIndex(ElementIdentifier);
  virtual void DeleteIndex(ElementIdentifier);
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkAutoVectorContainer.cxx"
#endif

#endif
