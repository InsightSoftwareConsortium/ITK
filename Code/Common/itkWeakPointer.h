/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWeakPointer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWeakPointer_h
#define __itkWeakPointer_h

#include "itkMacro.h"
#include <iostream>

namespace itk
{

/** \class WeakPointer
 * \brief Implements a weak reference to an object.
 *
 * WeakPointer implements a weak reference to an object. A natural
 * interface to the class is defined by operator -> (and *) among others.
 * WeakPointer is really nothing more than a standard pointer. It is used
 * to call attention to the fact that it does not adjust the reference
 * count of an object like SmartPointer does. WeakPointer is used internally
 * to Insight to manage (break) reference counting loops. At some point,
 * an object may keep track of how many objects hold weak references to
 * itself. In all cases, however, an outstanding weak reference will not
 * keep an object from destructing.
 *
 * \ingroup ITKSystemObjects
 * \ingroup DataAccess
 */
template <class TObjectType>
class ITK_EXPORT WeakPointer
{
public:
  /** Extract information from template parameter. */
  typedef TObjectType ObjectType;

  /** Constructor.  */
  WeakPointer ()
    { m_Pointer = 0; }

  /** Copy constructor.  */
  WeakPointer (const WeakPointer<ObjectType> &p): m_Pointer(p.m_Pointer) {}

  /** Constructor to pointer p.  */
  WeakPointer (ObjectType *p): m_Pointer(p) { }

  /** Destructor.  */
  ~WeakPointer ()
    { m_Pointer = 0; }

  /** Overload operator ->.  */
  ObjectType *operator -> () const
    { return m_Pointer; }

  /** Return pointer to object.  */
  operator ObjectType * () const
    { return m_Pointer; }

  /** Template comparison operators. */
  template <typename R>
  bool operator == (R r) const
    {
    return (m_Pointer == (ObjectType*)r);
    }
  template <typename R>
  bool operator != (R r) const
    {
    return (m_Pointer != (ObjectType*)r);
    }

  /** Access function to pointer. */
  ObjectType *GetPointer () const
    { return m_Pointer; }

  /** Comparison of pointers. Less than comparison.  */
  bool operator < (const WeakPointer &r) const
    { return (void*)m_Pointer < (void*) r.m_Pointer; }

  /** Comparison of pointers. Greater than comparison.  */
  bool operator > (const WeakPointer &r) const
    { return (void*)m_Pointer > (void*) r.m_Pointer; }

  /** Comparison of pointers. Less than or equal to comparison.  */
  bool operator <= (const WeakPointer &r) const
    { return (void*)m_Pointer <= (void*) r.m_Pointer; }

  /** Comparison of pointers. Greater than or equal to comparison.  */
  bool operator >= (const WeakPointer &r) const
    { return (void*)m_Pointer >= (void*) r.m_Pointer; }

  /** Overload operator assignment.  */
  WeakPointer &operator = (const WeakPointer &r)
    { return this->operator = (r.GetPointer()); }

  /** Overload operator assignment.  */
  WeakPointer &operator = (ObjectType *r)
    {
    m_Pointer = r;
    return *this;
    }

  /** Function to print object pointed to.  */
  ObjectType *Print (std::ostream& os) const
    {
    // This prints the object pointed to by the pointer
    (*m_Pointer).Print(os);
    return m_Pointer;
    }

private:
  /** The pointer to the object referrred to by this smart pointer. */
  ObjectType* m_Pointer;
};


template <typename T>
std::ostream& operator<< (std::ostream& os, WeakPointer<T> p)
{
  p.Print(os);
  return os;
}

} // end namespace itk

#endif
