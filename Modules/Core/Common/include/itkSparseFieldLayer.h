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
#ifndef itkSparseFieldLayer_h
#define itkSparseFieldLayer_h

#include "itkObjectFactory.h"
#include "itkObject.h"
#include <vector>

namespace itk
{
/**
 * \class ConstSparseFieldLayerIterator
 * \brief Used to iterate through an itkSparseFieldLayer.
 *
 *  This class is modelled on the STL Forward Iterator concept, and is used to
 *  iterate through an itkSparseFieldLayer.
 * \ingroup ITKCommon
 */
template< typename TNodeType >
class ITK_TEMPLATE_EXPORT ConstSparseFieldLayerIterator
{
public:
  const TNodeType & operator*() const
  { return *m_Pointer; }

  const TNodeType * operator->() const
  { return m_Pointer; }

  const TNodeType * GetPointer() const
  { return m_Pointer; }

  bool operator==(const ConstSparseFieldLayerIterator o) const
  {
    if ( m_Pointer == o.m_Pointer ) { return true; }
    else { return false; }
  }

  bool operator!=(const ConstSparseFieldLayerIterator o) const
  {
    if ( m_Pointer != o.m_Pointer ) { return true; }
    else { return false; }
  }

  ConstSparseFieldLayerIterator & operator++()
  {
    m_Pointer = m_Pointer->Next;
    return *this;
  }

  ConstSparseFieldLayerIterator & operator--()
  {
    m_Pointer = m_Pointer->Previous;
    return *this;
  }

  ConstSparseFieldLayerIterator()
  { m_Pointer = ITK_NULLPTR; }

  ConstSparseFieldLayerIterator(TNodeType *p)
  { m_Pointer = p; }

  ~ConstSparseFieldLayerIterator() {}

protected:
  TNodeType *m_Pointer;
};

/** \class SparseFieldLayerIterator
 *  \brief The non-const version of the ConstSparseFieldLayerIterator.
 * \ingroup ITKCommon
 */
template< typename TNodeType >
class ITK_TEMPLATE_EXPORT SparseFieldLayerIterator:
  public ConstSparseFieldLayerIterator< TNodeType >
{
public:
  typedef ConstSparseFieldLayerIterator< TNodeType > Superclass;

  SparseFieldLayerIterator():Superclass()
  {}

  SparseFieldLayerIterator(TNodeType *p):Superclass(p)
  {}

  TNodeType & operator*()
  { return *this->m_Pointer; }

  TNodeType * operator->()
  { return this->m_Pointer; }

  TNodeType * GetPointer()
  { return this->m_Pointer; }

  SparseFieldLayerIterator & operator++()
  {
    this->m_Pointer = this->m_Pointer->Next;
    return *this;
  }

  SparseFieldLayerIterator & operator--()
  {
    this->m_Pointer = this->m_Pointer->Previous;
    return *this;
  }

  SparseFieldLayerIterator & operator=(Superclass & sc)
  {
    this->m_Pointer = const_cast< TNodeType * >( sc.GetPointer() );
    return *this;
  }
};

/** \class SparseFieldLayer
 *  \brief A very simple linked list that is used to manage
 *         nodes in a layer of a sparse field level-set solver.
 *  \par
 *  This class implements a *very* simple linked list that is used to manage
 *  nodes in a layer of a sparse field level-set solver.  For more information
 *  on the sparse field level-set solver, see documentation for
 *  itk::SparseFieldLevelSetImageFilter.
 *
 *  \par IMPORTANT!
 *  One important and distinctive feature of this list implementation (in fact,
 *  the entire reason for this object's existence) is that no memory
 *  allocation/deallocation occurs during linking or unlinking of nodes.  The
 *  nodes themselves are expected to carry the appropriate "Next" & "Previous"
 *  fields used to link.  Guaranteeing that no calls to new or delete are
 *  *ever* made for normal list operations allows us to safely use this class
 *  in a multithread environment without incurring penalties from heap
 *  contention among threads. Because no allocation/deallocation occurs, it is
 *  entirely up to the calling program to manage the allocating and freeing of
 *  the list nodes.
 * \ingroup ITKCommon
 */
template< typename TNodeType >
class ITK_TEMPLATE_EXPORT SparseFieldLayer:
  public Object
{
public:
  /** Standard typedefs. */
  typedef SparseFieldLayer           Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Method for creation through the object factory. */
  itkNewMacro(Self);

  /** Run-time type information (and related methods). */
  itkTypeMacro(SparseFieldLayer, Object);

  /** Type of node stored in the linked list. */
  typedef TNodeType NodeType;

  /** Alias for the type of value stored in the list. Conforms to Standard Template
   *  Library vocabulary. */
  typedef NodeType ValueType;

  /** Iterator type for the list. */
  typedef SparseFieldLayerIterator< NodeType > Iterator;

  /** Const iterator type for the list. */
  typedef ConstSparseFieldLayerIterator< NodeType > ConstIterator;

  /** Regions used for multithreading */
  struct RegionType {
    ConstIterator first;
    ConstIterator last;  // this is one past the actual last element
  };

  typedef std::vector< RegionType > RegionListType;

  /** Returns a pointer to the first node in the list.  Constant
   * time. */
  NodeType * Front()
  { return m_HeadNode->Next; }

  /** Returns a const pointer to the first node in the list. Constant time. */
  const NodeType * Front() const
  { return m_HeadNode->Next; }

  /** Unlinks the first node from the list. Constant time. */
  void PopFront()
  {
    m_HeadNode->Next = m_HeadNode->Next->Next;
    m_HeadNode->Next->Previous = m_HeadNode;
    m_Size -= 1;
  }

  /** Links a node into the front of the list. Constant time. */
  void PushFront(NodeType *n)
  {
    n->Next = m_HeadNode->Next;
    n->Previous = m_HeadNode;
    m_HeadNode->Next->Previous = n;
    m_HeadNode->Next = n;
    m_Size += 1;
  }

  /** Unlinks a node from the list */
  void Unlink(NodeType *n)
  {
    n->Previous->Next = n->Next;
    n->Next->Previous = n->Previous;
    m_Size -= 1;
  }

  /** Returns an iterator pointing to the first node in the list. */
  Iterator Begin()
  { return Iterator(m_HeadNode->Next); }

  /** Returns a const iterator pointing to the first node in the
   * list. */
  ConstIterator Begin() const
  { return ConstIterator(m_HeadNode->Next); }

  /** Returns an iterator pointing one node past the end of the list. */
  Iterator End()
  { return Iterator(m_HeadNode); }

  /** Returns a const iterator pointing one node past the end of the list. */
  ConstIterator End() const
  { return ConstIterator(m_HeadNode); }

  /** Returns TRUE if the list is empty, FALSE otherwise. Executes in constant
   *  time. */
  bool Empty() const
  {
    if ( m_HeadNode->Next == m_HeadNode ) { return true; }
    else { return false; }
  }

  /** Returns the number of elements in the list. Size() executes in constant
   *  time. */
  unsigned int Size() const;

  /** Returns pointers to first and last+1 elements of num partitions of
      the itkSparseFieldLayer */
  RegionListType SplitRegions(int num) const;

protected:
  SparseFieldLayer();
  ~SparseFieldLayer() ITK_OVERRIDE;
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

private:
  ITK_DISALLOW_COPY_AND_ASSIGN(SparseFieldLayer);

  /** The anchor node of the list.  m_HeadNode->Next is the first node in the
   *  list. If m_HeadNode->Next == m_HeadNode, then the list is empty. */
  NodeType *   m_HeadNode;
  unsigned int m_Size;
};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSparseFieldLayer.hxx"
#endif

#endif
