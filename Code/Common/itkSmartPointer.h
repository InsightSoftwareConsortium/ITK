/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSmartPointer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkSmartPointer_h
#define __itkSmartPointer_h

#include "itkMacro.h"
#include "itkWeakPointer.h"
#include <iostream>

namespace itk
{

/** \class SmartPointer
 * \brief Implements transparent reference counting.
 *
 * SmartPointer implements reference counting by overloading
 * operator -> (and *) among others. This allows natural interface
 * to the class referred to by the pointer without having to invoke
 * special Register()/UnRegister() methods directly.
 *
 * To compile / test this class
 * Windows: cl SmartPointerTest.cxx; .\SmartPointerTest.exe
 * linux:   c++ SmartPointerTest.cxx ./a.out
 * other:   CCcompiler SmartPointerTest.cxx  ./a.out
 *
 * \ingroup ITKSystemObjects
 * \ingroup DataAccess
 */
template <class TObjectType>
class ITK_EXPORT SmartPointer 
{
public:
  typedef TObjectType ObjectType;
  
  /** Constructor  */
  SmartPointer () 
    { m_Pointer = 0; }

  /** Copy constructor  */
  SmartPointer (const SmartPointer<ObjectType> &p):
    m_Pointer(p.m_Pointer)
    { this->Register(); }
  
  /** Constructor to pointer p  */
  SmartPointer (ObjectType *p):
    m_Pointer(p)
    { this->Register(); }                             
  
  /** Construct from a WeakPointer */
  SmartPointer (const WeakPointer<ObjectType> &p)
    {
    m_Pointer = p.GetPointer();
    this->Register(); 
    }
  
  /** Destructor  */
  ~SmartPointer ()
    {
    this->UnRegister();
    m_Pointer = 0;  
    }
  
  /** Overload operator ->  */
  ObjectType *operator -> () const
    { return m_Pointer; }

  /** Return pointer to object.  */
  operator ObjectType * () const 
    { return m_Pointer; }
  
  /** Test if the pointer has been initialized */
  bool IsInitialized() const
  { return m_Pointer != 0; }
  bool IsNull() const
  { return m_Pointer == 0; }

  /** Template comparison operators. */
  template <typename R>
  bool operator == ( R r ) const
    { return (m_Pointer == static_cast<ObjectType*>(r) ); }

  template <typename R>
  bool operator != ( R r ) const
    { return (m_Pointer != static_cast<ObjectType*>(r) ); }
    
  /** Access function to pointer. */
  ObjectType *GetPointer () const 
    { return m_Pointer; }
  
  /** Comparison of pointers. Less than comparison.  */
  bool operator < (const SmartPointer &r) const
    { return (void*)m_Pointer < (void*) r.m_Pointer; }
  
  /** Comparison of pointers. Greater than comparison.  */
  bool operator > (const SmartPointer &r) const
    { return (void*)m_Pointer > (void*) r.m_Pointer; }

  /** Comparison of pointers. Less than or equal to comparison.  */
  bool operator <= (const SmartPointer &r) const
    { return (void*)m_Pointer <= (void*) r.m_Pointer; }

  /** Comparison of pointers. Greater than or equal to comparison.  */
  bool operator >= (const SmartPointer &r) const
    { return (void*)m_Pointer >= (void*) r.m_Pointer; }

  /** Overload operator assignment.  */
  SmartPointer &operator = (const SmartPointer &r)
    { return this->operator = (r.GetPointer()); }
  
  /** Overload operator assignment.  */
  SmartPointer &operator = (const WeakPointer<ObjectType> &r)
    { return this->operator = (r.GetPointer()); }
  
  /** Overload operator assignment.  */
  SmartPointer &operator = (ObjectType *r)
    {                                                              
    if (m_Pointer != r)
      {
      ObjectType* tmp = m_Pointer; //avoid recursive unregisters by retaining temporarily
      m_Pointer = r;
      this->Register();
      if ( tmp ) { tmp->UnRegister(); }
      }
    return *this;
    }
  
  /** Function to print object pointed to  */
  ObjectType *Print (std::ostream& os) const 
    { 
      // This prints the object pointed to by the pointer  
      (*m_Pointer).Print(os);  
      return m_Pointer;
    } 

private:
  /** The pointer to the object referrred to by this smart pointer. */
  ObjectType* m_Pointer;

  void Register()
    { 
    if(m_Pointer) { m_Pointer->Register(); }
    }
  
  void UnRegister()
    {
    if(m_Pointer) { m_Pointer->UnRegister(); }
    }
};  

  
template <typename T>
std::ostream& operator<< (std::ostream& os, SmartPointer<T> p) 
{
  p.Print(os); 
  return os;
}

} // end namespace itk
  
#endif
