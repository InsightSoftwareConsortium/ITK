/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMapContainer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkMapContainer_h
#define __itkMapContainer_h

#include "itkObject.h"
#include "itkObjectFactory.h"

#include <map>

namespace itk
{

/** \class MapContainer
 * Define a front-end to the STL "map" container that conforms to the
 * IndexedContainerInterface.  This is a full-fleged Object, so
 * there are events, modification time, debug, and reference count 
 * information.
 *
 * Template parameters for MapContainer:
 *
 * TElementIdentifier =
 *    A type that shall be used to index the container.
 *    It must have a < operator defined for ordering.
 *
 * TElement =
 *    The element type stored in the container.
 *
 * \ingroup DataRepresentation
 */
template <typename TElementIdentifier, typename TElement>
class MapContainer:
  public Object,
  private std::map< TElementIdentifier , TElement >
{
public:
  /** Standard class typedefs. */
  typedef MapContainer        Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /** Standard part of every itk Object. */
  itkTypeMacro(MapContainer, Object);

  /** Save the template parameters. */
  typedef TElementIdentifier  ElementIdentifier;
  typedef TElement            Element;
    
private:
  MapContainer(const Self&); //purposely not implemented
  void operator=(const Self&); //purposely not implemented

  /** Quick access to the STL map type that was inherited. */
  typedef std::map<ElementIdentifier, Element>     MapType;
  typedef typename MapType::iterator               MapIterator;
  typedef typename MapType::const_iterator         MapConstIterator;
  typedef typename MapType::key_compare            MapKeyCompareType;
    
public:
  /** Provide pass-through constructors corresponding to all the STL
   * map constructors.  These are for internal use only since this is also
   * an Object which must be constructed through the "New()" routine. */
  MapContainer():MapType() {}
  MapContainer(const MapKeyCompareType& comp):MapType(comp) {}
  //  MapContainer(const Self& r):MapType(r) {}
  template <typename InputIterator>
  MapContainer(InputIterator first, InputIterator last):MapType(first, last) {}
  template <typename InputIterator>
  MapContainer(InputIterator first, InputIterator last,const MapKeyCompareType& comp):
    MapType(first, last, comp) {}  
  
  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** This type is provided to Adapt this container as an STL container */
  typedef MapType STLContainerType;

  /** Cast the container to a STL container type */
  STLContainerType & CastToSTLContainer() {
     return dynamic_cast<STLContainerType &>(*this); }

  /** Cast the container to a const STL container type */
  const STLContainerType & CastToSTLConstContainer() const {
     return dynamic_cast<const STLContainerType &>(*this); }

  /** Declare iterators to container. */
  class Iterator;
  class ConstIterator;
  friend class Iterator;
  friend class ConstIterator;
  
  /** \brief The non-const iterator type for the map. */
  class Iterator
  {
  public:
    Iterator() {}
    Iterator( const MapIterator& i ): m_Iter(i) {}
    
    Iterator& operator* ()    { return *this; }
    Iterator* operator-> ()   { return this; }
    Iterator& operator++ ()   { ++m_Iter; return *this; }
    Iterator operator++ (int) { Iterator temp(*this);  ++m_Iter; return temp; }
    Iterator& operator-- ()   { --m_Iter; return *this; }
    Iterator operator-- (int) { Iterator temp(*this); --m_Iter; return temp; }

    bool operator == (const Iterator& r) const { return m_Iter == r.m_Iter; }
    bool operator != (const Iterator& r) const { return m_Iter != r.m_Iter; }
    bool operator == (const ConstIterator& r) const { return m_Iter == r.m_Iter; }
    bool operator != (const ConstIterator& r) const { return m_Iter != r.m_Iter; }
 
    /** Get the index into the MapContainer associated with this iterator.   */
    ElementIdentifier Index(void) const { return m_Iter->first; }

    /** Get the value at this iterator's location in the MapContainer.   */
    Element& Value(void) { return m_Iter->second; }
  private:
    MapIterator      m_Iter;
    friend class     ConstIterator;
  };
  
  /** \brief The const iterator type for the map. */
  class ConstIterator
  {
  public:
    ConstIterator() {}
    ConstIterator(const MapConstIterator& ci): m_Iter(ci) {}
    ConstIterator(const Iterator& r) { m_Iter = r.m_Iter; }

    ConstIterator& operator* ()    { return *this; }
    ConstIterator* operator-> ()   { return this; }
    ConstIterator& operator++ ()   { ++m_Iter; return *this; }
    ConstIterator operator++ (int) { ConstIterator temp(*this);  ++m_Iter; return temp; }
    ConstIterator& operator-- ()   { --m_Iter; return *this; }
    ConstIterator operator-- (int) { ConstIterator temp(*this); --m_Iter; return temp; }

    bool operator == (const Iterator& r) const { return m_Iter == r.m_Iter; }
    bool operator != (const Iterator& r) const { return m_Iter != r.m_Iter; }
    bool operator == (const ConstIterator& r) const { return m_Iter == r.m_Iter; }
    bool operator != (const ConstIterator& r) const { return m_Iter != r.m_Iter; }
    
    /** Get the index into the MapContainer associated with this iterator.   */
    ElementIdentifier Index(void) const { return m_Iter->first; }

    /** Get the value at this iterator's location in the MapContainer.   */
    const Element& Value(void) const { return m_Iter->second; }

  private:
    MapConstIterator m_Iter;
    friend class Iterator;
  };

  /** Declare the public interface routines. */
  Element& ElementAt(ElementIdentifier);
  const Element& ElementAt(ElementIdentifier) const;
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
  void Initialize(void);
    
};

} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMapContainer.txx"
#endif

#endif
