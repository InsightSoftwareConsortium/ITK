/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMapContainer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkMapContainer_h
#define __itkMapContainer_h

#include <map>

#include "itkObject.h"
#include "itkSmartPointer.h"

/**
 * itkMapContainer
 *
 * Define a front-end to the STL "map" container that conforms to the
 * itkIndexedContainer interface.
 */
template <typename TElementIdentifier, typename TElement>
class itkMapContainer:
  public itkObject,
  public std::map< TElementIdentifier , TElement >
{
public:
  /**
   * Smart pointer typedef support.
   */
  typedef itkMapContainer        Self;
  typedef itkSmartPointer<Self>  Pointer;

  /**
   * Save the template parameters.
   */
  typedef TElementIdentifier  ElementIdentifier;
  typedef TElement            Element;
  
private:
  /**
   * Quick access to the STL map type that was inherited.
   */
  typedef std::map<ElementIdentifier, Element> Map;
  
protected:
  /**
   * Provide pass-through constructors corresponding to all the STL
   * map constructors.  These are for internal use only since this is also
   * an itkObject which must be constructed through the "New()" routine.
   */
  itkMapContainer():
    Map() {}

  itkMapContainer(const key_compare& comp):
    Map(comp) {}

  itkMapContainer(const Self& r):
    Map(r) {}
  
  template <typename InputIterator>
  itkMapContainer(InputIterator first, InputIterator last):
    Map(first, last) {}

  template <typename InputIterator>
  itkMapContainer(InputIterator first, InputIterator last,
		  const key_compare& comp):
    Map(first, last, comp) {}  
  
public:
  /**
   * Define types needed for the interface.
   */
  typedef Map::const_iterator  ConstIterator;
  
  /**
   * Declare the public interface routines.
   */
  static Pointer New(void);
  Element& ElementAt(ElementIdentifier);
  Element GetElement(ElementIdentifier) const;
  void SetElement(ElementIdentifier, Element);
  bool IndexExists(ElementIdentifier) const;
  bool GetElementIfIndexExists(ElementIdentifier, Element*) const;
  void CreateIndex(ElementIdentifier);
  void DeleteIndex(ElementIdentifier);
  ConstIterator Begin(void) const;
  ConstIterator End(void) const;  
};

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMapContainer.cxx"
#endif

#endif
