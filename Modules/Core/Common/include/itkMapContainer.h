/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/
#ifndef itkMapContainer_h
#define itkMapContainer_h

#include "itkObject.h"
#include "itkObjectFactory.h"

#include <map>

namespace itk
{
/** \class MapContainer
 * \brief A wrapper of the STL "map" container.
 *
 * Define a front-end to the STL "map" container that conforms to the
 * IndexedContainerInterface.  This is a full-fleged Object, so
 * there are events, modification time, debug, and reference count
 * information.
 *
 * \tparam TElementIdentifier A type that shall be used to index the
 * container. It must have a < operator defined for ordering.
 *
 * \tparam TElement The element type stored in the container.
 *
 * \ingroup DataRepresentation
 * \ingroup ITKCommon
 */
template< typename TElementIdentifier, typename TElement >
class ITK_TEMPLATE_EXPORT MapContainer:
  public Object,
  private std::map< TElementIdentifier, TElement >
{
public:
  /** Standard class typedefs. */
  typedef MapContainer               Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Standard part of every itk Object. */
  itkTypeMacro(MapContainer, Object);

  /** Save the template parameters. */
  typedef TElementIdentifier ElementIdentifier;
  typedef TElement           Element;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(MapContainer);

  /** Quick access to the STL map type that was inherited. */
  typedef std::map< ElementIdentifier, Element > MapType;
  typedef typename MapType::iterator             MapIterator;
  typedef typename MapType::const_iterator       MapConstIterator;
  typedef typename MapType::key_compare          MapKeyCompareType;

public:
  /** Provide pass-through constructors corresponding to all the STL
   * map constructors.  These are for internal use only since this is also
   * an Object which must be constructed through the "New()" routine. */
  MapContainer():MapType() {}
  MapContainer(const MapKeyCompareType & comp):MapType(comp) {}
  //  MapContainer(const Self& r):MapType(r) {}
  template< typename TInputIterator >
  MapContainer(TInputIterator first, TInputIterator last):MapType(first, last) {}
  template< typename TInputIterator >
  MapContainer(TInputIterator first, TInputIterator last, const MapKeyCompareType & comp):
    MapType(first, last, comp) {}

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** This type is provided to adapt this container as an STL container */
  typedef MapType STLContainerType;

  /** Cast the container to a STL container type */
  STLContainerType & CastToSTLContainer()
  { return dynamic_cast< STLContainerType & >( *this ); }

  /** Cast the container to a const STL container type */
  const STLContainerType & CastToSTLConstContainer() const
  {
    return dynamic_cast< const STLContainerType & >( *this );
  }

  using STLContainerType::begin;
  using STLContainerType::end;
  using STLContainerType::rbegin;
  using STLContainerType::rend;

  using STLContainerType::empty;
  using STLContainerType::size;
  using STLContainerType::max_size;

  using STLContainerType::operator[];

  using STLContainerType::insert;
  using STLContainerType::erase;
  using STLContainerType::swap;
  using STLContainerType::clear;

  using STLContainerType::key_comp;
  using STLContainerType::value_comp;

  using STLContainerType::find;
  using STLContainerType::count;
  using STLContainerType::lower_bound;
  using STLContainerType::upper_bound;
  using STLContainerType::equal_range;

  using STLContainerType::get_allocator;

  using typename STLContainerType::key_type;
  using typename STLContainerType::mapped_type;
  using typename STLContainerType::value_type;
  using typename STLContainerType::key_compare;
  using typename STLContainerType::value_compare;
  using typename STLContainerType::allocator_type;
  using typename STLContainerType::reference;
  using typename STLContainerType::const_reference;
  using typename STLContainerType::iterator;
  using typename STLContainerType::const_iterator;
  using typename STLContainerType::size_type;
  using typename STLContainerType::difference_type;
  using typename STLContainerType::pointer;
  using typename STLContainerType::const_pointer;
  using typename STLContainerType::reverse_iterator;
  using typename STLContainerType::const_reverse_iterator;

  /** Declare iterators to container. */
  class Iterator;
  class ConstIterator;
  friend class Iterator;
  friend class ConstIterator;

  /** \class Iterator
   * \brief The non-const iterator type for the map.
   * \ingroup ITKCommon
   */
  class Iterator
  {
public:
    typedef typename MapIterator::iterator_category iterator_category;
    typedef typename MapIterator::value_type        value_type;
    typedef typename MapIterator::difference_type   difference_type;
    typedef typename MapIterator::pointer           pointer;
    typedef typename MapIterator::reference         reference;

    Iterator() {}
    Iterator(const Iterator & i):m_Iter(i.m_Iter) {}
    Iterator(const MapIterator & i):m_Iter(i) {}
    Iterator & operator=(const Iterator & r ) { m_Iter = r.m_Iter; return *this; }

    Iterator & operator*()    { return *this; }
    Iterator * operator->()   { return this; }
    Iterator & operator++()   { ++m_Iter; return *this; }
    Iterator operator++(int) { Iterator temp(*this);  ++m_Iter; return temp; }
    Iterator & operator--()   { --m_Iter; return *this; }
    Iterator operator--(int) { Iterator temp(*this); --m_Iter; return temp; }

    bool operator==(const Iterator & r) const { return m_Iter == r.m_Iter; }
    bool operator!=(const Iterator & r) const { return m_Iter != r.m_Iter; }
    bool operator==(const ConstIterator & r) const { return m_Iter == r.m_Iter; }
    bool operator!=(const ConstIterator & r) const { return m_Iter != r.m_Iter; }

    /** Get the index into the MapContainer associated with this iterator.   */
    ElementIdentifier Index(void) const { return m_Iter->first; }

    /** Get the value at this iterator's location in the MapContainer.   */
    Element & Value(void) { return m_Iter->second; }

private:
    MapIterator m_Iter;
    friend class ConstIterator;
  };

  /** \class ConstIterator
   * \brief The const iterator type for the map.
   * \ingroup ITKCommon
   */
  class ConstIterator
  {
public:
    typedef typename MapConstIterator::iterator_category iterator_category;
    typedef typename MapConstIterator::value_type        value_type;
    typedef typename MapConstIterator::difference_type   difference_type;
    typedef typename MapConstIterator::pointer           pointer;
    typedef typename MapConstIterator::reference         reference;

    ConstIterator() {}
    ConstIterator(const MapConstIterator & ci):m_Iter(ci) {}
    ConstIterator(const Iterator & r) : m_Iter( r.m_Iter ) {}
    ConstIterator & operator=(const ConstIterator & r ) { m_Iter = r.m_Iter; return *this; }

    ConstIterator & operator*()    { return *this; }
    ConstIterator * operator->()   { return this; }
    ConstIterator & operator++()   { ++m_Iter; return *this; }
    ConstIterator operator++(int) { ConstIterator temp(*this);  ++m_Iter; return temp; }
    ConstIterator & operator--()   { --m_Iter; return *this; }
    ConstIterator operator--(int) { ConstIterator temp(*this); --m_Iter; return temp; }

    bool operator==(const Iterator & r) const { return m_Iter == r.m_Iter; }
    bool operator!=(const Iterator & r) const { return m_Iter != r.m_Iter; }
    bool operator==(const ConstIterator & r) const { return m_Iter == r.m_Iter; }
    bool operator!=(const ConstIterator & r) const { return m_Iter != r.m_Iter; }

    /** Get the index into the MapContainer associated with this iterator.   */
    ElementIdentifier Index(void) const { return m_Iter->first; }

    /** Get the value at this iterator's location in the MapContainer.   */
    const Element & Value(void) const { return m_Iter->second; }

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
  Element & ElementAt(ElementIdentifier);

  /**
   * Get a reference to the element at the given index.
   *
   */
  const Element & ElementAt(ElementIdentifier) const;

  /**
   * Get a reference to the element at the given index.
   * If the index does not exist, it is created automatically.
   *
   * It is assumed that the value of the element is modified through the
   * reference.
   */
  Element & CreateElementAt(ElementIdentifier);

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
  bool GetElementIfIndexExists(ElementIdentifier, Element *) const;

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
  ConstIterator Begin() const;

  /**
   * Get an end const iterator for the map.
   */
  ConstIterator End() const;

  /**
   * Get a begin const iterator for the map.
   */
  Iterator Begin();

  /**
   * Get an end const iterator for the map.
   */
  Iterator End();

  /**
   * Get the number of elements currently stored in the map.
   */
  ElementIdentifier Size() const;

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
  void Squeeze();

  /**
   * Tell the container to release any memory it may have allocated and
   * return itself to its initial state.
   */
  void Initialize();
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkMapContainer.hxx"
#endif

#endif
