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

/**
 * Define the "itkIndexedContainer" interface.
 */
template <typename TElementIdentifier, typename TElement>
class itkIndexedContainer: public itkObject
{
public:
  /**
   * Smart pointer typedef support.
   */
  typedef itkIndexedContainer      Self;
  typedef itkSmartPointer< Self >  Pointer;

  /**
   * Save the template parameters.
   */
  typedef TElementIdentifier  ElementIdentifier;
  typedef TElement            Element;
  
  /**
   * This is an "indexed" container, so we provide the indexing methods.
   */
  virtual Element GetElement(ElementIdentifier) const =0;
  virtual void SetElement(ElementIdentifier, Element)=0;
  
  /**
   * Test if there is an entry in the container corresponding to the given
   * index.
   */
  virtual bool IndexExists(ElementIdentifier) const =0;

  /**
   * Combine the GetElement and IndexExists into one method.
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
   */
  virtual void DeleteIndex(ElementIdentifier)=0;
  
  /**
   * Support const iteration operations through container.
   *
   * Dereferencing the iterator must produce a pair whose first member is the
   * element identifier, and whose second is the element itself.
   * This is similar to STL map iterators.
   */
  // typedef UnderlyingContainer::const_iterator  ConstIterator; 
  // ConstIterator Begin()
  // ConstIterator End()
};

#endif
