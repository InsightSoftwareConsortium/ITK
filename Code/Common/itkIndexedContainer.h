/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIndexedContainer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkIndexedContainer_h
#define __itkIndexedContainer_h

#include "itkObject.h"
#include "itkSmartPointer.h"

namespace itk
{

/** \class IndexedContainer
 * The "IndexedContainer" interface.  This should only be used for
 * reference when writing containers conforming to this interface.  ITK
 * uses generic programming to allow container type substitution, so
 * polymorphism is not needed to use containers through this interface.  This
 * means that a container conforming to this interface need not be derived
 * from it.  However, the container must derive from itkObject in order to
 * support the reference counting, modification time, and debug information
 * required by this interface.
 *
 * Note that many comments refer to a "default element" or "default element
 * value".  This value is equal to the default contstructor of the
 * Element type.  Also note that all non-const methods assume that the
 * container was modified, and update the modification time.
 *
 * Template parameters for IndexedContainer:
 *
 * TElementIdentifier =
 *    A type that shall be used to index the container.
 *    It must have a < operator defined for ordering.
 *
 * TElement =
 *    The element type stored in the container.
 */
template <typename TElementIdentifier, typename TElement>
class IndexedContainer: public itkObject
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef IndexedContainer      Self;
  
  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer< Self >  Pointer;

  /** \typedef
   * Save the template parameters.
   */
  typedef TElementIdentifier  ElementIdentifier;
  typedef TElement            Element;

  /**
   * This is an "indexed" container, so we provide the indexing methods.
   */
  
  /**
   * Get a reference to an existing element.
   * It is NOT guaranteed that the element will or will not be created if it
   * doesn't exist.  This behavior is implementation-specific.
   *
   * It is assumed that the value of the element is modified through the
   * reference.
   */
  virtual Element& ElementAt(ElementIdentifier)=0;

  /**
   * Get a reference to an existing element.
   * It is guaranteed that the element will be inserted with a default
   * value if it does not exist.
   *
   * It is assumed that the value of the element is modified through the
   * reference.
   */
  virtual Element& CreateElementAt(ElementIdentifier)=0;
  
  /**
   * Get a copy of an element without range checking.
   */
  virtual Element GetElement(ElementIdentifier) const =0;
  
  /**
   * Set the value of an element.
   * It is NOT guaranteed whether a spot for the element will be created
   * automatically.  This is implementation-defined.
   */
  virtual void SetElement(ElementIdentifier, Element)=0;
  
  /**
   * Set the value of an element.
   * It is guaranteed that a spot for the element will be created if it
   * doesn't exist.
   */
  virtual void InsertElement(ElementIdentifier, Element)=0;
  
  /**
   * Test if there is an entry in the container corresponding to the given
   * index.
   */
  virtual bool IndexExists(ElementIdentifier) const =0;

  /**
   * Combine the GetElement and IndexExists into one method.
   * If false is returned, then no element with the given identifier was found.
   * If true is returned, then the identifier was found.  In this case,
   * if the element pointer given as input is not null, the element is filled
   * in with the value of the element found.
   */
  virtual bool GetElementIfIndexExists(ElementIdentifier, Element*) const =0;
  
  /**
   * Create an entry in the container corresponding to the given index.
   * The entry will be initialized with the default element.
   * If an entry already exists, its value will be overwritten with the
   * default element.
   */
  virtual void CreateIndex(ElementIdentifier)=0;

  /**
   * Delete the entry in the container corresponding to the given identifier.
   *
   * It is NOT guaranteed that IndexExists(id) will return false if called
   * right after DeleteIndex(id).  This behavior is implementation-defined.
   * If the identifier's location is left behind, though, it will have the
   * value of the default element.
   */
  virtual void DeleteIndex(ElementIdentifier)=0;
  
  /**
   * Support const iteration operations through a container.
   *
   * Dereferencing the iterator must produce a pair whose first member is the
   * element identifier, and whose second is the element itself.
   * This is similar to STL map iterators.
   *
   * For front-ends to STL containers that already have a conforming iterator
   * type (like the STL map), the following typedef can be used instead
   * of creating an iterator by hand:
   *
   * typedef UnderlyingContainer::const_iterator  ConstIterator; 
   */
  class ConstIterator {}; 
  
  /**
   * Get a begin const iterator for the container.
   */  
  ConstIterator Begin()=0;
  
  /**
   * Get an end const iterator for the container.
   */
  ConstIterator End()=0;
};

} // namespace itk
  
#endif
