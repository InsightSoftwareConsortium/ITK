/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkTreeIteratorClone.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkTreeIteratorClone_h
#define __itkTreeIteratorClone_h

#include "itkMacro.h"
#include <iostream>

namespace itk
{
template< class TObjectType >
class ITK_EXPORT TreeIteratorClone
{
public:

  /** Typedefs */
  typedef TreeIteratorClone< TObjectType > Self;
  typedef TObjectType                      ObjectType;

  /** Constructor  */
  TreeIteratorClone ()
  {
    m_Pointer = 0;
  }

  /** Copy constructor  */
  TreeIteratorClone (const TreeIteratorClone< ObjectType > & p)
  {
    m_Pointer = 0;
    if ( p.m_Pointer != NULL )
      {
      m_Pointer = p.m_Pointer->Clone();
      }
  }

  /** Constructor to pointer p  */
  TreeIteratorClone (ObjectType *p)
  {
    m_Pointer = 0;
    if ( p != NULL )
      {
      m_Pointer = p->Clone();
      }
  }

  /** Constructor to reference p  */
  TreeIteratorClone (const ObjectType & p)
  {
    m_Pointer = 0;
    m_Pointer = const_cast< ObjectType * >( &p )->Clone();
  }

  /** Destructor  */
  ~TreeIteratorClone ()
  {
    delete m_Pointer;
    m_Pointer = 0;
  }

  /** Overload operator ->  */
  ObjectType * operator->() const
  { return m_Pointer; }

  /** Test if the pointer has been initialized */
  bool IsNotNull() const
  { return m_Pointer != 0; }
  bool IsNull() const
  { return m_Pointer == 0; }

  /** Template comparison operators. */
  template< typename TR >
  bool operator==(TR r) const
  { return ( m_Pointer == (ObjectType *)( r ) ); }

  template< typename TR >
  bool operator!=(TR r) const
  { return ( m_Pointer != (ObjectType *)( r ) ); }

  /** Access function to pointer. */
  ObjectType * GetPointer() const
  { return m_Pointer; }

  /** Comparison of pointers. Less than comparison.  */
  bool operator<(const TreeIteratorClone & r) const
  { return (void *)m_Pointer < (void *)r.m_Pointer; }

  /** Comparison of pointers. Greater than comparison.  */
  bool operator>(const TreeIteratorClone & r) const
  { return (void *)m_Pointer > (void *)r.m_Pointer; }

  /** Comparison of pointers. Less than or equal to comparison.  */
  bool operator<=(const TreeIteratorClone & r) const
  { return (void *)m_Pointer <= (void *)r.m_Pointer; }

  /** Comparison of pointers. Greater than or equal to comparison.  */
  bool operator>=(const TreeIteratorClone & r) const
  { return (void *)m_Pointer >= (void *)r.m_Pointer; }

  /** Overload operator assignment.  */
  TreeIteratorClone & operator=(const TreeIteratorClone & r)
  { return this->operator=( r.GetPointer() ); }

  /** Overload operator assignment.  */
  TreeIteratorClone & operator=(const ObjectType *r)
  {
    if ( m_Pointer != r )
      {
      delete m_Pointer;
      m_Pointer = 0;
      if ( r != NULL )
        {
        m_Pointer = const_cast< ObjectType * >( r )->Clone();
        }
      }
    return *this;
  }

  Self &
  operator++()
  {
    if ( m_Pointer )
      {
      ++( *m_Pointer );
      }
    return *this;
  }

  const Self
  operator++(int)
  {
    if ( m_Pointer )
      {
      const Self oldValue(m_Pointer);   // create a copy of the iterator behind
                                        // the pointer (Clone())
      ++( *m_Pointer );
      return oldValue;
      }
  }

  /** Function to print object pointed to  */
  ObjectType * Print(std::ostream & os) const
  {
    // This prints the object pointed to by the pointer
    ( *m_Pointer ).Print(os);
    return m_Pointer;
  }

private:
  /** The pointer to the object referrred to by this smart pointer. */
  ObjectType *m_Pointer;
};

template< typename T >
std::ostream & operator<<(std::ostream & os, TreeIteratorClone< T > p)
{
  p.Print(os);
  return os;
}
} // end namespace itk

#endif
