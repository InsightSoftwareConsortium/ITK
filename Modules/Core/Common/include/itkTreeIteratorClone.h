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
#ifndef itkTreeIteratorClone_h
#define itkTreeIteratorClone_h

#include "itkMacro.h"
#include <iostream>

namespace itk
{
/** \class itkTreeIteratorClone
 *  \brief itkTreeIteratorClone class
 *  \ingroup DataRepresentation
 *  \ingroup ITKCommon
 */
template< typename TObjectType >
class TreeIteratorClone
{
public:

  /** Typedefs */
  typedef TreeIteratorClone< TObjectType > Self;
  typedef TObjectType                      ObjectType;

  /** Constructor  */
  TreeIteratorClone () { m_Pointer = 0; }

  /** Copy constructor  */
  TreeIteratorClone (const TreeIteratorClone< ObjectType > & p)
  {
    m_Pointer = ITK_NULLPTR;
    if ( p.m_Pointer != ITK_NULLPTR )
      {
      m_Pointer = p.m_Pointer->Clone();
      }
  }

  /** Constructor to pointer p  */
  TreeIteratorClone (ObjectType *p)
  {
    m_Pointer = 0;
    if ( p != ITK_NULLPTR )
      {
      m_Pointer = p->Clone();
      }
  }

  /** Constructor to reference p  */
  TreeIteratorClone (const ObjectType & p)
  {
    m_Pointer = ITK_NULLPTR;
    m_Pointer = const_cast< ObjectType * >( &p )->Clone();
  }

  /** Destructor  */
  ~TreeIteratorClone ()
  {
    delete m_Pointer;
    m_Pointer = ITK_NULLPTR;
  }

  /** Overload operator ->  */
  ObjectType * operator->() const { return m_Pointer; }

  /** Test if the pointer has been initialized */
  bool IsNotNull() const { return m_Pointer != 0; }
  bool IsNull() const { return m_Pointer == 0; }

  /** Template comparison operators. */
  template< typename TR >
  bool operator==(TR r) const { return ( m_Pointer == (ObjectType *)( r ) ); }

  template< typename TR >
  bool operator!=(TR r) const { return ( m_Pointer != (ObjectType *)( r ) ); }

  /** Access function to pointer. */
  ObjectType * GetPointer() const { return m_Pointer; }

  /** Comparison of pointers. Less than comparison.  */
  bool operator<(const TreeIteratorClone & r) const { return (void *)m_Pointer < (void *)r.m_Pointer;  }

  /** Comparison of pointers. Greater than comparison.  */
  bool operator>(const TreeIteratorClone & r) const { return (void *)m_Pointer > (void *)r.m_Pointer; }

  /** Comparison of pointers. Less than or equal to comparison.  */
  bool operator<=(const TreeIteratorClone & r) const { return (void *)m_Pointer <= (void *)r.m_Pointer; }

  /** Comparison of pointers. Greater than or equal to comparison.  */
  bool operator>=(const TreeIteratorClone & r) const { return (void *)m_Pointer >= (void *)r.m_Pointer; }

  /** Overload operator assignment.  */
  TreeIteratorClone & operator=(const TreeIteratorClone & r) { return this->operator=( r.GetPointer() ); }

  /** Overload operator assignment.  */
  TreeIteratorClone & operator=(const ObjectType *r)
  {
    if ( m_Pointer != r )
      {
      delete m_Pointer;
      m_Pointer = ITK_NULLPTR;
      if ( r != ITK_NULLPTR )
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
  /** The pointer to the object referred to by this smart pointer. */
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
