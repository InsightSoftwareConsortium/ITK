/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkContainerInterfaces.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkContainerInterfaces_h
#define __itkContainerInterfaces_h

#include "itkObject.h"
#include "itkSmartPointer.h"

template <typename TElementIdentifier, typename TElement>
class itkUniqueAssociativeContainer: public itkObject
{
public:
  typedef itkUniqueAssociativeContainer  Self;
  typedef itkSmartPointer< Self >  	 Pointer;

  typedef TElementIdentifier  ElementIdentifier;
  typedef TElement            Element;
};


/**
 * Define the "itkIndexedContainer" interface.
 */
template <typename TElementIdentifier, typename TElement>
class itkIndexedContainer:
  public itkUniqueAssociativeContainer< TElementIdentifier , TElement >
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
   * This is an "indexed" container, so we proved the indexing operator.
   */
  virtual Element& operator[](const ElementIdentifier&)=0;
  
  /**
   * Test if there is an entry in the container corresponding to the given
   * index.
   */
  virtual bool IndexExists(const ElementIdentifier&)=0;
  
  /**
   * Create an entry in the container corresponding to the given index.
   * The entry will be initialized with the default element.
   * If an entry already exists, its value will be overwritten with the
   * default element.
   */
  virtual void CreateIndex(const ElementIdentifier&)=0;

  /**
   * Delete the entry in the container corresponding to the given identifier.
   */
  virtual void DeleteIndex(const ElementIdentifier&)=0;
};

#endif
