/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWatershedSegmentTree.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWatershedSegmentTree_h
#define __itkWatershedSegmentTree_h

#include "itkObjectFactory.h"
#include "itkDataObject.h"
#include "itkProcessObject.h"
#include <deque>
#include <functional>

namespace itk
{
namespace watershed
{
/* \class SegmentTree
 * A data structure for storing segment merge information used in filters of  
 * the watershed segmentation algorithm.  See itk::WatershedImageFilter for an
 * overview.
 *
 * \par
 * This class is the implemenation of the ``merge tree'' referred to in the
 * documentation for itk::WatershedImageFilter and other watershed segmentation 
 * component classes.  It holds a list of merges among image segments at
 * various saliency levels. The list is actually a representation of a binary
 * tree, whose nodes are segments and edges are saliencies.
 * \ingroup WatershedSegmentation
 * \sa itk::WatershedImageFilter */
template <class TScalarType>
class ITK_EXPORT SegmentTree : public DataObject
{
public:
  /** Define itk Smart Pointers for this object */
  typedef SegmentTree Self;
  typedef DataObject Superclass;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  itkNewMacro(Self);
  itkTypeMacro(SegmentTree, DataObject);
  typedef TScalarType ScalarType;

  /** Elements of the list (nodes of the tree).  A record of a merge
   * between two segments (unsigned long labels) at a particular
   * saliency.   */
  struct merge_t
  {
    unsigned long from;
    unsigned long to;
    ScalarType saliency;
  };

  /** Define the container type used in this list data structure */
  typedef std::deque<merge_t> DequeType;
  typedef typename DequeType::iterator Iterator;
  typedef typename DequeType::const_iterator ConstIterator;
  typedef typename DequeType::value_type ValueType;
    
 /** Boolean comparison functor for use in sorting functions.  */
  struct merge_comp : public std::binary_function<bool, const merge_t&,
                      const merge_t& >
  {
    bool operator()(const merge_t &a, const merge_t &b)
      {
        return b.saliency < a.saliency;
      }
  };
  
  /** Boolean comparison functor for use in sorting functions.   */
  struct sort_comp : public std::binary_function<bool, const merge_t&,
                     const merge_t& >
  {
    bool operator()(const merge_t &a, const merge_t &b)
      {
        return a.saliency < s.Saliency;
      }
  };

  /** Returns the size of the list.   */
  typename DequeType::size_type Size() const
    { return m_Deque.size(); }

  /** Returns TRUE if the SegmentTree is empty, FALSE if the SegmentTree is not
   * empty.    */
  bool Empty() const
    { return m_Deque.empty();    }

  /** Returns a const reference to the front of the list (node with the least
   * saliency value).    */
  const merge_t &Front() const
    { return m_Deque.front(); }

  /** Returns a const reference to the back of the list (node with the greatest
   * saliency value).   */
  const merge_t &Back() const
    { return m_Deque.back(); } 

  /** Returns a reference to the front of the list   */
  merge_t &Front()
    { return m_Deque.front(); }

  /** Returns a reference to the back of the list   */
  merge_t &Back()
    { return m_Deque.back(); }

  /** Inserts a node at the front of the list.   */
  void PushFront(const ValueType &t)
    { m_Deque.push_front(t); }

  /** Inserts a node at the back of the list   */
  void PushBack( const ValueType &t)
    { m_Deque.push_back(t); }

  /** Erases the node at the front of the list.   */
  void PopFront()
    { m_Deque.pop_front(); }

  /** Erases the node at the back of the list.   */
  void PopBack()
    { m_Deque.pop_back(); }

  /** Returns an iterator pointing to the first element in the list.   */
  Iterator Begin()
    { return m_Deque.begin(); }

  /** Returns a const iterator pointing to the first element in the list. */
  ConstIterator Begin() const
    { return m_Deque.begin(); }

  /** Returns an iterator pointing one element past the last element in the
   * list.    */
  Iterator End()
    { return m_Deque.end(); }

  /** Returns a const iterator pointing one element past the last element in the
   * list.    */
  ConstIterator End() const
    { return m_Deque.end(); }

  /** Clears the Deque completely */
  void Clear()
  { m_Deque.clear(); }
  
  /** Helper method for debugging.   */
  //  void PrintDeque();
protected:
  SegmentTree() {}
  virtual ~SegmentTree() {}
  SegmentTree(const Self&) {}
  void operator=(const Self&) {}
  void PrintSelf(std::ostream& os, Indent indent) const;

  DequeType m_Deque;
  void UpdateOutputInformation();
  bool VerifyRequestedRegion() { return true; }
  void SetRequestedRegionToLargestPossibleRegion () {}
  bool RequestedRegionIsOutsideOfTheBufferedRegion () { return false; }
  void SetRequestedRegion (itk::DataObject *) {}
};
}// end namespace watershed
}// end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkWatershedSegmentTree.txx"
#endif

#endif

