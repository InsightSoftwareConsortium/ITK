/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkSmartPointer.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef __itkSmartPointer_h
#define __itkSmartPointer_h

#include "itkMacro.h"
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
 */
template <class T>
class ITK_EXPORT SmartPointer 
{
public:
  /** 
   * Constructor 
   */
  SmartPointer () 
    {
    m_Pointer = 0;
    }

  /** 
   * Const constructor 
   */
  SmartPointer (const SmartPointer<T> &p)
    { 
    m_Pointer = p.m_Pointer; 
    this->Register(); 
    }
  
  /** 
   * Constructor to pointer p 
   */
  SmartPointer (T *p)
    { 
    m_Pointer = p; 
    this->Register(); 
    }                             

  /** 
   * Destructor 
   */
  ~SmartPointer ()
    {
    this->UnRegister();
    }

  /** 
   * Overload operator -> 
   */
  T *operator -> () const
    { 
    return m_Pointer; 
    }

  /** 
    * Return pointer to object.
    */
   operator T * () const 
     { 
     return m_Pointer; 
     }

  /** 
    * Return pointer to object.
    */
   operator T * ()  
     { 
     return m_Pointer; 
     }

   /*
    * Template comparison operators.
    */
   template <typename R>
   bool operator == (R r) const
     {
       return (m_Pointer == (T*)r);
     }
   template <typename R>
   bool operator != (R r) const
     {
       return (m_Pointer != (T*)r);
     }

  /** 
   * Access function to pointer.
   */
  T *GetPointer () const 
    { 
    return m_Pointer; 
    }
  
  /** 
   * Comparison of pointers. Less than comparison. 
   */
  bool operator < (const SmartPointer &r)
    { 
    return (void*)m_Pointer < (void*) r.m_Pointer; 
    }

  /** 
   * Comparison of pointers. Greater than comparison. 
   */
  bool operator > (const SmartPointer &r)
    { 
    return (void*)m_Pointer > (void*) r.m_Pointer; 
    }

  /** 
   * Comparison of pointers. Less than or equal to comparison. 
   */
  bool operator <= (const SmartPointer &r)
    { 
    return (void*)m_Pointer <= (void*) r.m_Pointer; 
    }

  /** 
   * Comparison of pointers. Greater than or equal to comparison. 
   */
  bool operator >= (const SmartPointer &r)
    { 
    return (void*)m_Pointer >= (void*) r.m_Pointer; 
    }

  /** 
   * Overload operator assignment. 
   */
  SmartPointer &operator = (const SmartPointer &r)
    { 
    return this->operator = (r.GetPointer()); 
    }
  
  /** 
   * Overload operator assignment. 
   */
  SmartPointer &operator = (T *r)
    {                                                              
    if (m_Pointer != r)
      {
      this->UnRegister();
      m_Pointer = r;
      this->Register();
      }
    return *this;
    }
  
  /** 
   * Function to print object pointed to 
   */
  T *Print (std::ostream& os) const 
    { 
    // This prints the object pointed to by the pointer  
    (*m_Pointer).Print(os);  
    return m_Pointer;
    } 

private:
  /* The pointer to the object referrred to by this smart pointer. */
  T* m_Pointer;

  void Register()
    { 
    if (m_Pointer)
      {
      m_Pointer->Register();
      }
    }
  
  void UnRegister()
    {
    if (m_Pointer)
      {
      m_Pointer->UnRegister();
      }
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
