/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkVectorContainer.h
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
#ifndef __itkVectorContainer_h
#define __itkVectorContainer_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkSmartPointer.h"

#include <utility>
#include <vector>

namespace itk
{

/** \class VectorContainer
 * Define a front-end to the STL "vector" container that conforms to the
 * IndexedContainerInterface.  This is a full-fleged Object, so
 * there is modification time, debug, and reference count information.
 *
 * Template parameters for VectorContainer:
 *
 * TElementIdentifier =
 *     An INTEGRAL type for use in indexing the vector.
 *
 * TElement =
 *    The element type stored in the container.
 */
  
template <
  typename TElementIdentifier,
  typename TElement
  >
class VectorContainer: 
  public Object,
  private std::vector<TElement>
{
public:
  /**
   * Standard "Self" typedef.
   */
  typedef VectorContainer     Self;
  
  /**
   * Standard "Superclass" typedef.
   */
  typedef Object  Superclass;

  /**
   * Smart pointer typedef support.
   */
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  /**
   * Save the template parameters.
   */
  typedef TElementIdentifier  ElementIdentifier;
  typedef TElement            Element;
  

private:
  /**
   * Quick access to the STL vector type that was inherited.
   */
  typedef std::vector<Element>                VectorType;
  typedef typename VectorType::size_type          size_type;  
  typedef typename VectorType::iterator           VectorIterator;
  typedef typename VectorType::const_iterator     VectorConstIterator;
  
  
protected:
  /**
   * Provide pass-through constructors corresponding to all the STL
   * vector constructors.  These are for internal use only since this is also
   * an Object which must be constructed through the "New()" routine.
   */
  
  /**
   *
   */
  VectorContainer():
    Object(), VectorType() {}
  
  /**
   *
   */
  VectorContainer(size_type n):
    Object(), VectorType(n) {}
  
  /**
   *
   */
  VectorContainer(size_type n, const Element& x):
    Object(), VectorType(n, x) {}
  
  /**
   *
   */
  VectorContainer(const Self& r):
    Object(), VectorType(r) {}
  
  /**
   *
   */
  template <typename InputIterator>
  VectorContainer(InputIterator first, InputIterator last):
    Object(), VectorType(first, last) {}

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
   * Simulate STL-map style iteration where dereferencing the iterator
   * gives access to both the index and the value.
   */
  class Iterator
  {
  public:
    Iterator() {}
    Iterator(size_type d, const VectorIterator& i): m_Pos(d), m_Iter(i) {}
    
    Iterator& operator* ()    { return *this; }
    Iterator* operator-> ()   { return this; }
    Iterator& operator++ ()   { ++m_Pos; ++m_Iter; return *this; }
    Iterator operator++ (int) { Iterator temp(*this); ++m_Pos; ++m_Iter; return temp; }
    Iterator& operator-- ()   { --m_Pos; --m_Iter; return *this; }
    Iterator operator-- (int) { Iterator temp(*this); --m_Pos; --m_Iter; return temp; }

    bool operator == (const Iterator& r) const { return m_Iter == r.m_Iter; }
    bool operator != (const Iterator& r) const { return m_Iter != r.m_Iter; }
    bool operator == (const ConstIterator& r) const { return m_Iter == r.m_Iter; }
    bool operator != (const ConstIterator& r) const { return m_Iter != r.m_Iter; }
    
    /**
     * Get the index into the VectorContainer associated with this iterator.
     */
    ElementIdentifier Index(void) const { return m_Pos; }
    
    /**
     * Get the value at this iterator's location in the VectorContainer.
     */
    Element& Value(void) const { return *m_Iter; }
    
  private:
    size_type m_Pos;
    VectorIterator m_Iter;
    friend class ConstIterator;
  };

  
  /**
   * Simulate STL-map style const iteration where dereferencing the iterator
   * gives read access to both the index and the value.
   */
  class ConstIterator
  {
  public:
    ConstIterator() {}
    ConstIterator(size_type d, const VectorConstIterator& i): m_Pos(d), m_Iter(i) {}
    ConstIterator(const Iterator& r) { m_Pos = r.m_Pos; m_Iter = r.m_Iter; }
    
    ConstIterator& operator* ()    { return *this; }
    ConstIterator* operator-> ()   { return this; }
    ConstIterator& operator++ ()   { ++m_Pos; ++m_Iter; return *this; }
    ConstIterator operator++ (int) { ConstIterator temp(*this); ++m_Pos; ++m_Iter; return temp; }
    ConstIterator& operator-- ()   { --m_Pos; --m_Iter; return *this; }
    ConstIterator operator-- (int) { ConstIterator temp(*this); --m_Pos; --m_Iter; return temp; }

    Iterator& operator = (Iterator& r) { m_Pos = r.m_Pos; m_Iter = r.m_Iter; return r; }
    
    bool operator == (const Iterator& r) const { return m_Iter == r.m_Iter; }
    bool operator != (const Iterator& r) const { return m_Iter != r.m_Iter; }
    bool operator == (const ConstIterator& r) const { return m_Iter == r.m_Iter; }
    bool operator != (const ConstIterator& r) const { return m_Iter != r.m_Iter; }
    
    /**
     * Get the index into the VectorContainer associated with this iterator.
     */
    ElementIdentifier Index(void) const { return m_Pos; }
    
    /**
     * Get the value at this iterator's location in the VectorContainer.
     */
    const Element& Value(void) const { return *m_Iter; }
    
  private:
    size_type m_Pos;
    VectorConstIterator m_Iter;
    friend class Iterator;
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
  void Clear(void);
  
  /**
   * Standard part of every itk Object.
   */
  itkTypeMacro(VectorContainer, Object);
};

} // end namespace itk
  
#ifndef ITK_MANUAL_INSTANTIATION
#include "itkVectorContainer.txx"
#endif

#endif
