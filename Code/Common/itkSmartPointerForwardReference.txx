/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkSmartPointerForwardReference.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "itkSmartPointerForwardReference.h"

namespace itk
{

//----------------------------------------------------------------------------
template <class T>
inline SmartPointerForwardReference<T>
::SmartPointerForwardReference (const SmartPointerForwardReference<T> &p)
{ 
  m_Pointer = p.m_Pointer; 
  this->Register(); 
}
  
//----------------------------------------------------------------------------
template <class T>
inline SmartPointerForwardReference<T>
::SmartPointerForwardReference (T *p)
{ 
  m_Pointer = p; 
  this->Register(); 
}                             
  
//----------------------------------------------------------------------------
template <class T>
inline SmartPointerForwardReference<T>
::~SmartPointerForwardReference ()
{
  this->UnRegister();
}
  
//----------------------------------------------------------------------------
template <class T>
inline T *
SmartPointerForwardReference<T>
::operator -> () const
{ 
  return m_Pointer; 
}

//----------------------------------------------------------------------------
template <class T>
inline SmartPointerForwardReference<T>
::operator T * () const 
{ 
  return m_Pointer; 
}
  
//----------------------------------------------------------------------------
template <class T>
inline T *
SmartPointerForwardReference<T>
::GetPointer () const 
{ 
  return m_Pointer; 
}
  
//----------------------------------------------------------------------------
template <class T>
inline bool 
SmartPointerForwardReference<T>
::operator < (const SmartPointerForwardReference &r)
{ 
  return (void*)m_Pointer < (void*) r.m_Pointer; 
}

//----------------------------------------------------------------------------
template <class T>
inline bool 
SmartPointerForwardReference<T>
::operator > (const SmartPointerForwardReference &r)
{ 
  return (void*)m_Pointer > (void*) r.m_Pointer; 
}

//----------------------------------------------------------------------------
template <class T>
inline bool 
SmartPointerForwardReference<T>
::operator <= (const SmartPointerForwardReference &r)
{ 
  return (void*)m_Pointer <= (void*) r.m_Pointer; 
}

//----------------------------------------------------------------------------
template <class T>
inline bool 
SmartPointerForwardReference<T>
::operator >= (const SmartPointerForwardReference &r)
{ 
  return (void*)m_Pointer >= (void*) r.m_Pointer; 
}

//----------------------------------------------------------------------------
template <class T>
SmartPointerForwardReference<T> &
SmartPointerForwardReference<T>
::operator = (const SmartPointerForwardReference &r)
{ 
  return this->operator = (r.GetPointer()); 
}
  
//----------------------------------------------------------------------------
template <class T>
inline SmartPointerForwardReference<T> &
SmartPointerForwardReference<T>
::operator = (T *r)
{                                                              
  if (m_Pointer != r)
    {
    T* tmp = m_Pointer; //avoid recursive unregisters by retaining temporarily
    m_Pointer = r;
    this->Register();
    if ( tmp )
      {
      tmp->UnRegister();
      }
    }
  return *this;
}
  
//----------------------------------------------------------------------------
template <class T>
T *
SmartPointerForwardReference<T>
::Print (std::ostream& os) const 
{ 
  // This prints the object pointed to by the pointer  
  (*m_Pointer).Print(os);  
  return m_Pointer;
} 

//----------------------------------------------------------------------------
template <class T>
inline void 
SmartPointerForwardReference<T>
::Register()
{ 
  if (m_Pointer)
    {
    m_Pointer->Register();
    }
}
  
//----------------------------------------------------------------------------
template <class T>
inline void 
SmartPointerForwardReference<T>
::UnRegister()
{
  if (m_Pointer)
    {
    m_Pointer->UnRegister();
    }
}

} //end namespace
