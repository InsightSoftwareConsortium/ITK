/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkNeighborhoodOperatorList.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkNeighborhoodOperatorList_h
#define __itkNeighborhoodOperatorList_h

#include "itkNeighborhoodOperator.h"
#include <list>

namespace itk
{
/**
 * \class NeighborhoodOperatorList
 * \brief A linked list of NeighborhoodOperators.
 *
 * NeighborhoodOperatorList is a linked list of NeighborhoodOperators.
 *
 */
  
template <class TPixel, unsigned int VDimension=2>
class ITK_EXPORT NeighborhoodOperatorList
{
public:
  /**
   * Standard Self typedef
   */
  typedef NeighborhoodOperatorList<TPixel, VDimension> Self;

  /**
   * NeighborhoodOperator typedef support
   */
  typedef NeighborhoodOperator<TPixel, VDimension> NeighborhoodOperator;

  /**
   * Stl list typedef support
   */
  typedef list<NeighborhoodOperator *> list;

  /**
   * Iterator typedef support.
   */
  typedef list::iterator Iterator;

  /**
   * Returns an iterator that points to the beginning of the list.
   */
  Iterator Begin() { return m_List.begin(); }

  /**
   * Returns an iterator that points to one element past the end
   * of the list.
   */
  Iterator End()   { return m_List.end();   }

  /**
   * Returns the number of elements contained in the list.
   */
  std::size_t Size() const { return m_List.size(); }

  /**
   * Returns a boolean value indicating whether or not the list is
   * empty.
   */
  bool Empty() const { return m_List.empty(); }

  /**
   * Returns a pointer reference to the first element in the list.
   * This function must return a pointer to preserve polymorphism.
   */
  NeighborhoodOperator *& Front()     { return m_List.front(); }

  /**
   * Returns a pointer reference to the last element in the list.
   * This function must return a pointer to preserve polymorphism.
   */
  NeighborhoodOperator *& Back()      { return m_List.back() ; }

  /**
   * Removes the first element of the list.
   */
  void PopFront() { m_List.pop_front(); }

  /**
   * Removes the last element of the list.
   */
  void PopBack()  { m_List.pop_back();  }

  /**
   * Adds an element to the end of the list.
   */
  void PushBack(NeighborhoodOperator &o)
  {
    m_List.push_back(CopyOperator(o));
  }

  /**
   * Adds an element to the front of the list.
   */
  void PushFront(NeighborhoodOperator &o)
  {
    m_List.push_front(CopyOperator(o));
  }

  /**
   * Empties the list.  Frees any allocated memory.
   */
  void Clear();
  
  /**
   * Constructor.
   */
  NeighborhoodOperatorList() {};

  /**
   * Copy constructor.
   */
  NeighborhoodOperatorList(const NeighborhoodOperatorList &orig)
  {
    *this = orig;
  }

  /**
   * Assignement operator.  Relies on the user-defined virtual
   * functions Copy() and New() in the NeighborhoodOperator subclasses.
   */
  NeighborhoodOperatorList &operator=(const NeighborhoodOperatorList &);

  /**
   * Destructor.
   */
  ~NeighborhoodOperatorList();

  /**
   * Print some debugging information. ---Remove this function-- jc 10-9-00
   */
  void Print()
  {
    list::iterator it = m_List.begin();
    while(it != m_List.end())
      {
        (*it)->Print();
        ++it;
      }
  }
private:
  /**
   * Returns a pointer to a newly allocated operator that is of the same type
   * as the argument.
   */
  NeighborhoodOperator *CopyOperator(NeighborhoodOperator &) const;

  /**
   * The stl list structure that is used to maintain a list of
   * pointers to operators that are allocated by the push functions.
   */
  list m_List;

};
} // end namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkNeighborhoodOperatorList.txx"
#endif

#endif
