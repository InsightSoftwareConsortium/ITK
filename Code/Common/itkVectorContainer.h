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
#ifndef __itkVectorContainer_h
#define __itkVectorContainer_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkSmartPointer.h"

#include <utility>
#include <vector>

namespace itk
{

/** \class VectorContainer
 * Define a front-end to the STL "vector" container that conforms to the
 * IndexedContainerInterface.  This is a full-fleged Object, so
 * there is modification time, debug, and reference count information.
 *
 * Template parameters for VectorContainer:
 *
 * TElementIdentifier =
 *     An INTEGRAL type for use in indexing the vector.
 *
 * TElement =
 *    The element type stored in the container.
 */
  
template <
  typename TElementIdentifier,
  typename TElement
  >
class VectorContainer: 
  public Object,
  private std::vector<TElement>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef VectorContainer     Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  
  /** \typedef
   * Save the template parameters.
   */
  typedef TElementIdentifier  ElementIdentifier;
  typedef TElement            Element;
  

private:
  /**
   * Quick access to the STL vector type that was inherited.
   */
  typedef std::vector<Element>  Vector;
  typedef typename Vector::size_type     size_type;  
  typedef typename Vector::iterator           VectorIterator;
  typedef typename Vector::const_iterator     VectorConstIterator;
  
  
protected:
  /**
   * Provide pass-through constructors corresponding to all the STL
   * vector constructors.  These are for internal use only since this is also
   * an Object which must be constructed through the "New()" routine.
   */
  
  /**
   *
   */
  VectorContainer():
    Vector() {}
  
  /**
   *
   */
  VectorContainer(size_type n):
    Vector(n) {}
  
  /**
   *
   */
  VectorContainer(size_type n, const Element& x):
    Vector(n, x) {}
  
  /**
   *
   */
  VectorContainer(const Self& r):
    Vector(r) {}
  
  /**
   *
   */
  template <typename InputIterator>
  VectorContainer(InputIterator first, InputIterator last):
    Vector(first, last) {}

public:
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  class Iterator;
  class ConstIterator;
  friend class Iterator;
  friend class ConstIterator;
  

  /**
   * Simulate STL-map style iteration where dereferencing the iterator
   * gives access to both the index and the value.
   */
  class Iterator
  {
  public:
    Iterator() {}
    Iterator(size_type d, const VectorIterator& i): m_Pos(d), m_Iter(i) {}
    
    Iterator& operator* ()    { return *this; }
    Iterator* operator-> ()   { return this; }
    Iterator& operator++ ()   { ++m_Pos; ++m_Iter; return *this; }
    Iterator operator++ (int) { Iterator temp(*this); ++m_Pos; ++m_Iter; return temp; }
    Iterator& operator-- ()   { --m_Pos; --m_Iter; return *this; }
    Iterator operator-- (int) { Iterator temp(*this); --m_Pos; --m_Iter; return temp; }

    bool operator == (const Iterator& r) const { return m_Iter == r.m_Iter; }
    bool operator != (const Iterator& r) const { return m_Iter != r.m_Iter; }
    bool operator == (const ConstIterator& r) const { return m_Iter == r.m_Iter; }
    bool operator != (const ConstIterator& r) const { return m_Iter != r.m_Iter; }
    
    /**
     * Get the index into the VectorContainer associated with this iterator.
     */
    ElementIdentifier Index(void) const { return m_Pos; }
    
    /**
     * Get the value at this iterator's location in the VectorContainer.
     */
    Element& Value(void) { return *m_Iter; }
    
  private:
    size_type m_Pos;
    VectorIterator m_Iter;
    friend class ConstIterator;
  };

  
  /**
   * Simulate STL-map style const iteration where dereferencing the iterator
   * gives read access to both the index and the value.
   */
  class ConstIterator
  {
  public:
    ConstIterator() {}
    ConstIterator(size_type d, const VectorConstIterator& i): m_Pos(d), m_Iter(i) {}
    ConstIterator(const Iterator& r) { m_Pos = r.m_Pos; m_Iter = r.m_Iter; }
    
    ConstIterator& operator* ()    { return *this; }
    ConstIterator* operator-> ()   { return this; }
    ConstIterator& operator++ ()   { ++m_Pos; ++m_Iter; return *this; }
    ConstIterator operator++ (int) { ConstIterator temp(*this); ++m_Pos; ++m_Iter; return temp; }
    ConstIterator& operator-- ()   { --m_Pos; --m_Iter; return *this; }
    ConstIterator operator-- (int) { ConstIterator temp(*this); --m_Pos; --m_Iter; return temp; }

    Iterator& operator = (Iterator& r) { m_Pos = r.m_Pos; m_Iter = r.m_Iter; return r; }
    
    bool operator == (const Iterator& r) const { return m_Iter == r.m_Iter; }
    bool operator != (const Iterator& r) const { return m_Iter != r.m_Iter; }
    bool operator == (const ConstIterator& r) const { return m_Iter == r.m_Iter; }
    bool operator != (const ConstIterator& r) const { return m_Iter != r.m_Iter; }
    
    /**
     * Get the index into the VectorContainer associated with this iterator.
     */
    ElementIdentifier Index(void) const { return m_Pos; }
    
    /**
     * Get the value at this iterator's location in the VectorContainer.
     */
    const Element& Value(void) const { return *m_Iter; }
    
  private:
    size_type m_Pos;
    VectorConstIterator m_Iter;
    friend class Iterator;
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
  Iterator Begin(void);
  Iterator End(void);  
  unsigned long Size(void) const;
  void Reserve(ElementIdentifier);
  void Squeeze(void);
  
  /**
   * Standard part of every itk Object.
   */
  itkTypeMacro(VectorContainer, Object);
};

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorContainer.txx"
#endif

#endif
