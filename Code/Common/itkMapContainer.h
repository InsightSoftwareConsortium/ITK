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

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkSmartPointer.h"

#include <map>

namespace itk
{

/** \class MapContainer
 * Define a front-end to the STL "map" container that conforms to the
 * IndexedContainerInterface.  This is a full-fleged Object, so
 * there is modification time, debug, and reference count information.
 *
 * Template parameters for MapContainer:
 *
 * TElementIdentifier =
 *    A type that shall be used to index the container.
 *    It must have a < operator defined for ordering.
 *
 * TElement =
 *    The element type stored in the container.
 */
template <typename TElementIdentifier, typename TElement>
class MapContainer:
  public Object,
  public std::map< TElementIdentifier , TElement >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef MapContainer        Self;
  
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
   * Quick access to the STL map type that was inherited.
   */
  typedef std::map<ElementIdentifier, Element> Map;
  
protected:
  /**
   * Provide pass-through constructors corresponding to all the STL
   * map constructors.  These are for internal use only since this is also
   * an Object which must be constructed through the "New()" routine.
   */
  
  /**
   *
   */
  MapContainer():
    Map() {}

  /**
   *
   */
  MapContainer(const key_compare& comp):
    Map(comp) {}

  /**
   *
   */
  MapContainer(const Self& r):
    Map(r) {}
  
  /**
   *
   */
  template <typename InputIterator>
  MapContainer(InputIterator first, InputIterator last):
    Map(first, last) {}

  /**
   *
   */
  template <typename InputIterator>
  MapContainer(InputIterator first, InputIterator last,
		  const key_compare& comp):
    Map(first, last, comp) {}  
  
public:
  /**
   * Method for creation through the object factory.
   */
  itkNewMacro(Self);
  
  /**
   * The const iterator type for the map.
   */
  typedef Map::const_iterator  ConstIterator;
  /**
   * The non-const iterator type for the map.
   */
  typedef Map::iterator  Iterator;
  
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
  itkTypeMacro(MapContainer, Object);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMapContainer.txx"
#endif

#endif
