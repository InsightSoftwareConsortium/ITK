/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSparseFieldLayer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSparseFieldLayer_h
#define __itkSparseFieldLayer_h

#include "itkObjectFactory.h"
#include "itkObject.h"
#include <vector>

namespace itk {

/**
 * \class ConstSparseFieldLayerIterator
 *  This class is modeled on the STL Forward Iterator concept, and is used to
 *  iterate through an itkSparseFieldLayer.
 */
template <class TNodeType>
class ConstSparseFieldLayerIterator
{
public:
  const TNodeType& operator*() const
  { return *m_Pointer; }

  const TNodeType* operator->() const
  { return m_Pointer; }

  const TNodeType* GetPointer() const
  { return m_Pointer; }
  
  bool operator==(const ConstSparseFieldLayerIterator o) const
  {
    if (m_Pointer == o.m_Pointer) return true;
    else return false;
  }

  bool operator!=(const ConstSparseFieldLayerIterator o) const
  {
    if (m_Pointer != o.m_Pointer) return true;
    else return false;
  }

  ConstSparseFieldLayerIterator &operator++()
  {
    m_Pointer = m_Pointer->Next;
    return *this;
  }

  ConstSparseFieldLayerIterator &operator--()
  {
    m_Pointer = m_Pointer->Previous;
    return *this;
  }

  ConstSparseFieldLayerIterator()
  { m_Pointer=0; }

  ConstSparseFieldLayerIterator(TNodeType *p)
  { m_Pointer=p; }

  ~ConstSparseFieldLayerIterator() {}

protected:
  TNodeType *m_Pointer;
};

/** \class SparseFieldLayerIterator
 *  The non-const version of the ConstSparseFieldLayerIterator.
 */
template <class TNodeType>
class SparseFieldLayerIterator
  : public ConstSparseFieldLayerIterator<TNodeType>
{
public:
  typedef ConstSparseFieldLayerIterator<TNodeType> Superclass;

  SparseFieldLayerIterator() : Superclass()
  { }

  SparseFieldLayerIterator(TNodeType *p) : Superclass(p)
  { }

  TNodeType &operator*()
  { return *m_Pointer; }

  TNodeType* operator->()
  { return m_Pointer; }

  TNodeType* GetPointer()
  { return m_Pointer; }
  
  SparseFieldLayerIterator &operator++()
  {
    m_Pointer = m_Pointer->Next;
    return *this;
  }

  SparseFieldLayerIterator &operator--()
  {
    m_Pointer = m_Pointer->Previous;
    return *this;
  }

  SparseFieldLayerIterator &operator=(Superclass &sc)
  {
    m_Pointer = const_cast<TNodeType*> (sc.GetPointer());
    return *this;
  }
};


/** \class itkSparseFieldLayer
 *      
 *  \par
 *  This class implements a *very* simple linked list that is used to manage
 *  nodes in a layer of a sparse field level-set solver.  For more information
 *  on the sparse field level-set solver, see documentation for
 *  itk::SparseFieldLevelSetImageFilter.
 *
 *  \par IMPORTANT!
 *  One important and distinctive feature of this list implementation (in fact,
 *  the entire reason for this object's existance) is that no memory
 *  allocation/deallocation occurs during linking or unlinking of nodes.  The
 *  nodes themselves are expected to carry the appropriate "Next" & "Previous"
 *  fields used to link.  Guaranteeing that no calls to new or delete are
 *  *ever* made for normal list operations allows us to safely use this class
 *  in a multithread environment without incurring penalties from heap
 *  contention among threads. Because no allocation/deallocation occurs, it is
 *  entirely up to the calling program to manage the allocating and freeing of
 *  the list nodes. 
 */
template <class TNodeType>
class ITK_EXPORT SparseFieldLayer
  : public Object
{
public:
  /** Standard typedefs. */
  typedef SparseFieldLayer Self;
  typedef Object  Superclass;
  typedef SmartPointer<Self>  Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

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
  typedef SparseFieldLayerIterator<NodeType> Iterator;

  /** Const iterator type for the list.*/
  typedef ConstSparseFieldLayerIterator<NodeType> ConstIterator;

  /** Regions used for multithreading */
  struct RegionType 
  {
    ConstIterator first;
    ConstIterator last;  // this is one past the actual last element
  };   
  
  typedef std::vector<RegionType> RegionListType;

  /** Returns a pointer to the first node in the list.  Constant time.*/ 
  NodeType *Front()
  { return m_HeadNode->Next; }

  /** Returns a const pointer to the first node in the list. Constant time. */
  const NodeType *Front() const
  { return m_HeadNode->Next; }

  /** Unlinks the first node from the list. Constant time. */
  void PopFront()
  {
    m_HeadNode->Next = m_HeadNode->Next->Next;
    m_HeadNode->Next->Previous = m_HeadNode;
  }

  /** Links a node into the front of the list. Constant time. */
  void PushFront(NodeType *n)
  {
    n->Next = m_HeadNode->Next;
    n->Previous = m_HeadNode;
    m_HeadNode->Next->Previous = n;
    m_HeadNode->Next = n;
  }

  /** Unlinks a node from the list */
  void Unlink(NodeType *n)
  {
    n->Previous->Next = n->Next;
    n->Next->Previous = n->Previous;
  }
  
  /** Returns an iterator pointing to the first node in the list. */
  Iterator Begin()
  { return Iterator(m_HeadNode->Next); }

  /** Returns a const iterator pointing to the first node in the list.*/
  ConstIterator Begin() const
  { return ConstIterator(m_HeadNode->Next); }

  /** Returns an iterator pointing one node past the end of the list.*/
  Iterator End()
  { return Iterator(m_HeadNode); }

  /** Returns a const iterator pointing one node past the end of the list. */
  ConstIterator End() const
  { return ConstIterator(m_HeadNode); }
  
  /** Returns TRUE if the list is empty, FALSE otherwise. Executes in constant
   *  time. */
  bool Empty() const
  {
    if (m_HeadNode->Next == m_HeadNode) return true;
    else return false;
  }

  /** Returns the number of elements in the list.  The usual STL warning
   *  applies here: There are no guarantees that Size() executes in constant
   *  time; the implementation may be order N time.  To test for an empty
   *  list, use Empty() instead of Size()==0. */
  unsigned int Size() const;
  
  /** Returns pointers to first and last+1 elements of num partitions of 
      the itkSparseFieldLayer */
  RegionListType SplitRegions(int num) const;

  /** Splices the contents of another SparseFieldLayer into this one, combining
      the two lists.*/
  //  void Splice (SparseFieldLayer &);
  
protected:
  SparseFieldLayer();
  ~SparseFieldLayer();
  virtual void PrintSelf(std::ostream& os, Indent indent) const;
  
private:
  SparseFieldLayer(const Self&);    //purposely not implemented
  void operator=(const Self&);      //purposely not implemented
  
  /** The anchor node of the list.  m_HeadNode->Next is the first node in the
   *  list. If m_HeadNode->Next == m_HeadNode, then the list is empty. */
  NodeType *m_HeadNode; 
};


} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkSparseFieldLayer.txx"
#endif

#endif
