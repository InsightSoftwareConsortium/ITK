/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkMapContainer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

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
