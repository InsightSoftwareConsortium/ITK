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
/**
 * MapContainer
 *
 * Define a front-end to the STL "map" container that conforms to the
 * IndexedContainer interface.  This is a full-fleged itkObject, so
 * there is modification time, debug, and reference count information.
 */
#ifndef __itkMapContainer_h
#define __itkMapContainer_h

#include <map>

#include "itkObject.h"
#include "itkSmartPointer.h"

namespace itk
{

/**
 * Template parameters for MapContainer:
 *
 * TElementIdentifier =
 *    A type that shall be used to index the container.
 *    It must have a < operator defined for ordering.
 * TElement =
 *    The element type stored in the container.
 */

template <typename TElementIdentifier, typename TElement>
class MapContainer:
  public itkObject,
  public std::map< TElementIdentifier , TElement >
{
public:
  /**
   * Smart pointer typedef support.
   */
  typedef MapContainer           Self;
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
  MapContainer():
    Map() {}

  MapContainer(const key_compare& comp):
    Map(comp) {}

  MapContainer(const Self& r):
    Map(r) {}
  
  template <typename InputIterator>
  MapContainer(InputIterator first, InputIterator last):
    Map(first, last) {}

  template <typename InputIterator>
  MapContainer(InputIterator first, InputIterator last,
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
#include "itkMapContainer.cxx"
#endif

#endif
