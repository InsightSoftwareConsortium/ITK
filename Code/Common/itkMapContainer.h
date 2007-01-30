/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMapContainer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
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
  public std::map< TElementIdentifier , TElement >
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

  /* Declare the public interface routines. */

  /**
   * Get a reference to the element at the given index.
   * If the index does not exist, it is created automatically.
   *
   * It is assumed that the value of the element is modified through the
   * reference.
   */
  Element& ElementAt(ElementIdentifier);

  /**
   * Get a reference to the element at the given index.
   *
   */
  const Element& ElementAt(ElementIdentifier) const;

  /**
   * Get a reference to the element at the given index.
   * If the index does not exist, it is created automatically.
   *
   * It is assumed that the value of the element is modified through the
   * reference.
   */
  Element& CreateElementAt(ElementIdentifier);

  /**
   * Get the element at the specified index.  There is no check for
   * existence performed.
   */
  Element GetElement(ElementIdentifier) const;

  /**
   * Set the given index value to the given element.  If the index doesn't
   * exist, it is automatically created.
   */
  void SetElement(ElementIdentifier, Element);

  /**
   * Set the given index value to the given element.  If the index doesn't
   * exist, it is automatically created.
   */
  void InsertElement(ElementIdentifier, Element);

  /**
   * Check if the STL map has an entry corresponding to the given index.
   * The count will be either 1 or 0.
   */
  bool IndexExists(ElementIdentifier) const;

  /**
   * If the given index doesn't exist in the map, return false.
   * Otherwise, set the element through the pointer (if it isn't null), and
   * return true.
   */
  bool GetElementIfIndexExists(ElementIdentifier, Element*) const;

  /**
   * The map will create an entry for a given index through the indexing
   * operator.  Whether or not it is created, it will be assigned to the
   * default element.
   */
  void CreateIndex(ElementIdentifier);

  /**
   * Delete the entry in the STL map corresponding to the given identifier.
   * If the entry does not exist, nothing happens.
   */
  void DeleteIndex(ElementIdentifier);

  /**
   * Get a begin const iterator for the map.
   */
  ConstIterator Begin(void) const;

  /**
   * Get an end const iterator for the map.
   */
  ConstIterator End(void) const;  

  /**
   * Get a begin const iterator for the map.
   */
  Iterator Begin(void);

  /**
   * Get an end const iterator for the map.
   */
  Iterator End(void);  
  
  /**
   * Get the number of elements currently stored in the map.
   */
  unsigned long Size(void) const;

  /**
   * Tell the container to allocate enough memory to allow at least
   * as many elements as the size given to be stored.  This is NOT
   * guaranteed to actually allocate any memory, but is useful if the
   * implementation of the container allocates contiguous storage.
   */
  void Reserve(ElementIdentifier);

  /**
   * Tell the container to try to minimize its memory usage for storage of
   * the current number of elements.  This is NOT guaranteed to decrease
   * memory usage.
   */
  void Squeeze(void);

  /**
   * Tell the container to release any memory it may have allocated and
   * return itself to its initial state.
   */
  void Initialize(void);
    
};

} // end namespace itk

// Define instantiation macro for this template.
#define ITK_TEMPLATE_MapContainer(_, EXPORT, x, y) namespace itk { \
  _(2(class EXPORT MapContainer< ITK_TEMPLATE_2 x >)) \
  namespace Templates { typedef MapContainer< ITK_TEMPLATE_2 x > \
                                                  MapContainer##y; } \
  }

#if ITK_TEMPLATE_EXPLICIT
# include "Templates/itkMapContainer+-.h"
#endif

#if ITK_TEMPLATE_TXX
# include "itkMapContainer.txx"
#endif

#endif
