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
  private std::map< TElementIdentifier , TElement >
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef MapContainer        Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;

  /**
   * Save the template parameters.
   */
  typedef TElementIdentifier  ElementIdentifier;
  typedef TElement            Element;
  
private:
  /**
   * Quick access to the STL map type that was inherited.
   */
  typedef std::map<ElementIdentifier, Element>     MapType;
  typedef typename MapType::iterator               MapIterator;
  typedef typename MapType::const_iterator         MapConstIterator;
  typedef typename MapType::key_compare            MapKeyCompareType;
  
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
    MapType() {}

  /**
   *
   */
  MapContainer(const MapKeyCompareType& comp):
    MapType(comp) {}

  /**
   *
   */
  MapContainer(const Self& r):
    MapType(r) {}
  
  /**
   *
   */
  template <typename InputIterator>
  MapContainer(InputIterator first, InputIterator last):
    MapType(first, last) {}

  /**
   *
   */
  template <typename InputIterator>
  MapContainer(InputIterator first, InputIterator last,
		  const MapKeyCompareType& comp):
    MapType(first, last, comp) {}  
  
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
   * The non-const iterator type for the map.
   */
  class Iterator: public MapIterator
  {
  public:
    Iterator(const MapIterator& i): MapIterator(i) {}
    Iterator& operator* ()    { return *this; }
    Iterator* operator-> ()   { return this; }
    /**
     * Get the index into the MapContainer associated with this iterator.
     */
    ElementIdentifier Index(void) const { return (*this)->first; }
    /**
     * Get the value at this iterator's location in the MapContainer.
     */
    Element& Value(void) { return (*this)->second; }
  };
  
  /**
   * The const iterator type for the map.
   */
  class ConstIterator: public MapConstIterator
  {
  public:
    ConstIterator(const MapConstIterator& ci): MapConstIterator(ci) {}
    ConstIterator& operator* ()    { return *this; }
    ConstIterator* operator-> ()   { return this; }
    /**
     * Get the index into the MapContainer associated with this iterator.
     */
    ElementIdentifier Index(void) const { return (*this)->first; }
    /**
     * Get the value at this iterator's location in the MapContainer.
     */
    const Element& Value(void) const { return (*this)->second; }
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
  itkTypeMacro(MapContainer, Object);
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMapContainer.txx"
#endif

#endif
