/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkAutoPointer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkAutoPointer_h
#define __itkAutoPointer_h

#include "itkMacro.h"
#include <iostream>

namespace itk
{

/** \class AutoPointer
 * \brief Implements an Automatic Pointer to an object.
 *
 * AutoPointer is intended to facilitate the construction of 
 * objects on-the-fly for those objects that are not to be shared.
 * An AutopOinter destroys its object when it goes out of scope.
 * Ownership of the object is transferred from one AutoPointer
 * to another AutoPointer when the assignement operator is used.
 * AutoPointers can release the ownership of the object they
 * hold
 *
 * This class follows the design of the std::auto_ptr class, the 
 * main reason for not using the std version is to avoid the use
 * of templated methods which greately difficult wrapping for
 * Tcl, Python and Java.
 *
 * \ingroup ITKSystemObjects
 * \ingroup DataAccess
 */
template <class TObjectType>
class ITK_EXPORT AutoPointer 
{
public:
  /** Extract information from template parameter. */
  typedef TObjectType   ObjectType;
  typedef AutoPointer   Self;
  
  /** Constructor.  */
  AutoPointer ():
    m_Pointer(0)
    { }

  /** Copy constructor.  */
  AutoPointer ( const Self & p ):
                   m_Pointer(p.Release())
    { }


  /** Constructor to pointer p.  */
  AutoPointer (ObjectType *p):
    m_Pointer(p) { }                             
  
  /** Destructor.  */
  ~AutoPointer ()
    { if( m_Pointer ) 
        { 
        delete m_Pointer;
        }
      m_Pointer = 0; }
  
  /** Overload operator ->.  */
  ObjectType *operator -> () const
    { return m_Pointer; }

  /** Return pointer to object.  */
  operator ObjectType * () const 
    { return m_Pointer; }
  
  /** Clear the AutoPointer. If it had a pointer the object
      is deleted and the pointer is set to null. */
  void Reset( void ) const
    {
    if( m_Pointer ) 
      { 
      delete m_Pointer;
      }
    m_Pointer = 0;
    }

  /** Release the pointer hold by the current AutoPointer 
      and return the raw pointer so it can be hold by 
      another AutoPointer. This operation is intended to
      be used for facilitating polymorphism.
  
      Example: if class Cow derives from Mammal,
      AutoPointer<Cow> onecow = new Cow;
      AutoPointer<Mammal> onemammal = onecow.Release(); */
  ObjectType * Release( void ) const 
    {
    ObjectType * oldpointer = m_Pointer;
    m_Pointer = 0;
    return oldpointer;
    }

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
  bool operator < (const AutoPointer &r)
    { return (void*)m_Pointer < (void*) r.m_Pointer; }
  
  /** Comparison of pointers. Greater than comparison.  */
  bool operator > (const AutoPointer &r)
    { return (void*)m_Pointer > (void*) r.m_Pointer; }

  /** Comparison of pointers. Less than or equal to comparison.  */
  bool operator <= (const AutoPointer &r)
    { return (void*)m_Pointer <= (void*) r.m_Pointer; }

  /** Comparison of pointers. Greater than or equal to comparison.  */
  bool operator >= (const AutoPointer &r)
    { return (void*)m_Pointer >= (void*) r.m_Pointer; }

  /** Overload operator assignment.  */
  AutoPointer &operator = (const AutoPointer &r)
    { this->operator = (r.Release()); return *this; }
  
  /** Overload operator assignment.  */
  AutoPointer &operator = (ObjectType *r)
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
  mutable  ObjectType* m_Pointer;
};  

  
template <typename T>
std::ostream& operator<< (std::ostream& os, AutoPointer<T> p) 
{
  p.Print(os); 
  return os;
}

} // end namespace itk
  
#endif
