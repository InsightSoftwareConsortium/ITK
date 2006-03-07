/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorContainer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkVectorContainer_h
#define __itkVectorContainer_h

#include "itkObject.h"
#include "itkObjectFactory.h"

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
 *
 * \ingroup DataRepresentation
 */
template <
  typename TElementIdentifier,
  typename TElement
  >
class ITK_EXPORT VectorContainer: 
  public Object,
  public std::vector<TElement>
{
public:
  /** Standard class typedefs. */
  typedef VectorContainer     Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Save the template parameters. */
  typedef TElementIdentifier  ElementIdentifier;
  typedef TElement            Element;
  
private:
  /** Quick access to the STL vector type that was inherited. */
  typedef std::vector<Element>                VectorType;
  typedef typename VectorType::size_type          size_type;  
  typedef typename VectorType::iterator           VectorIterator;
  typedef typename VectorType::const_iterator     VectorConstIterator;
    
protected:
  /** Provide pass-through constructors corresponding to all the STL
   * vector constructors.  These are for internal use only since this is also
   * an Object which must be constructed through the "New()" routine. */
  VectorContainer():
    Object(), VectorType() {}
  VectorContainer(size_type n):
    Object(), VectorType(n) {}
  VectorContainer(size_type n, const Element& x):
    Object(), VectorType(n, x) {}
  VectorContainer(const Self& r):
    Object(), VectorType(r) {}
  template <typename InputIterator>
  VectorContainer(InputIterator first, InputIterator last):
    Object(), VectorType(first, last) {}
  
public:

  /** This type is provided to Adapt this container as an STL container */
  typedef VectorType STLContainerType;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);
  
  /** Standard part of every itk Object. */
  itkTypeMacro(VectorContainer, Object);

  /** Convenient typedefs for the iterator and const iterator. */
  class Iterator;
  class ConstIterator;
    
  /** Cast the container to a STL container type */
  STLContainerType & CastToSTLContainer() {
     return dynamic_cast<STLContainerType &>(*this); }

  /** Cast the container to a const STL container type */
  const STLContainerType & CastToSTLConstContainer() const {
     return dynamic_cast<const STLContainerType &>(*this); }

  /** Friends to this class. */
  friend class Iterator;
  friend class ConstIterator;
  
  /** Simulate STL-map style iteration where dereferencing the iterator
   * gives access to both the index and the value. */
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
    
    /** Get the index into the VectorContainer associated with this iterator.   */
    ElementIdentifier Index(void) const { return static_cast<ElementIdentifier>( m_Pos ); }
    
    /** Get the value at this iterator's location in the VectorContainer.   */
    Element& Value(void) const { return *m_Iter; }
    
  private:
    size_type m_Pos;
    VectorIterator m_Iter;
    friend class ConstIterator;
  };
  
  /** Simulate STL-map style const iteration where dereferencing the iterator
   * gives read access to both the index and the value. */
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

    ConstIterator& operator = (const Iterator& r) { m_Pos = r.m_Pos; m_Iter = r.m_Iter; return *this; }
    
    bool operator == (const Iterator& r) const { return m_Iter == r.m_Iter; }
    bool operator != (const Iterator& r) const { return m_Iter != r.m_Iter; }
    bool operator == (const ConstIterator& r) const { return m_Iter == r.m_Iter; }
    bool operator != (const ConstIterator& r) const { return m_Iter != r.m_Iter; }
    
    /** Get the index into the VectorContainer associated with this iterator.   */
    ElementIdentifier Index(void) const { return static_cast<ElementIdentifier>( m_Pos ); }
    
    /** Get the value at this iterator's location in the VectorContainer.   */
    const Element& Value(void) const { return *m_Iter; }
    
  private:
    size_type m_Pos;
    VectorConstIterator m_Iter;
    friend class Iterator;
  };  
  
  /* Declare the public interface routines. */
  
  /**
   * Get a reference to the element at the given index.
   * It is assumed that the index exists, and it will not automatically
   * be created.
   *
   * It is assumed that the value of the element is modified through the
   * reference.
   */
  Element& ElementAt(ElementIdentifier);

  /**
   * Get a reference to the element at the given index.
   * It is assumed that the index exists, and it will not automatically
   * be created.
   *
   */
  const Element& ElementAt(ElementIdentifier) const;

  /**
   * Get a reference to the element at the given index.
   * If the element location does not exist, it will be created with a
   * default element value.
   *
   * It is assumed that the value of the element is modified through the
   * reference.
   */
  Element& CreateElementAt(ElementIdentifier);

  /**
   * Read the element from the given index.
   * It is assumed that the index exists.
   */
  Element GetElement(ElementIdentifier) const;

  /**
   * Set the element value at the given index.
   * It is assumed that the index exists.
   */
  void SetElement(ElementIdentifier, Element );

  /**
   * Set the element value at the given index.
   * If the element location does not exist, it will be created with a
   * default element value.
   */
  void InsertElement(ElementIdentifier, Element );

  /**
   * Check if the index range of the vector is large enough to allow the
   * given index without expansion.
   */
  bool IndexExists(ElementIdentifier) const;
  
  /**
   * Check if the given index is in range of the vector.  If it is not, return
   * false.  Otherwise, set the element through the pointer (if it isn't NULL),
   * and return true.
   */
  bool GetElementIfIndexExists(ElementIdentifier, Element*) const;

  /**
   * Make sure that the index range of the vector is large enough to allow
   * the given index, expanding it if necessary.  The index will contain
   * the default element regardless of whether expansion occured.
   */
  void CreateIndex(ElementIdentifier);

  /**
   * Delete the element defined by the index identifier.  In practice, it
   * doesn't make sense to delete a vector index.  Instead, this method just
   * overwrite the index with the default element.
   */
  void DeleteIndex(ElementIdentifier);
  
  /**
   * Get a begin const iterator for the vector.
   */
  ConstIterator Begin(void) const;

  /**
   * Get an end const iterator for the vector.
   */
  ConstIterator End(void) const;  

  /**
   * Get a begin iterator for the vector.
   */
  Iterator Begin(void);

  /**
   * Get an end iterator for the vector.
   */
  Iterator End(void);  

  /**
   * Get the number of elements currently stored in the vector.
   */
  unsigned long Size(void) const;

  /**
   * Tell the container to allocate enough memory to allow at least as many
   * elements as the size given to be stored.  In the generic case of ITK
   * containers this is NOT guaranteed to actually allocate any memory, but it
   * is useful if the implementation of the container allocates contiguous
   * storage. In the particular implementation of this VectorContainer the call
   * to this method actually allocates memory for the number of elements
   * defined by ElementIdentifier.
   */
  void Reserve( ElementIdentifier );

  /**
   * Tell the container to try to minimize its memory usage for storage of the
   * current number of elements.  This is NOT guaranteed to decrease memory
   * usage. This method is included here mainly for providing a unified API
   * with other containers in the toolkit.
   */
  void Squeeze(void);

  /**
   * Clear the elements. The final size will be zero.
   */
  void Initialize(void);
    
};

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorContainer.txx"
#endif

#endif
