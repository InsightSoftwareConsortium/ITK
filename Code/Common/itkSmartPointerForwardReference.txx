/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkSmartPointerForwardReference.txx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _itkSmartPointerForwardReference_txx
#define _itkSmartPointerForwardReference_txx
#include "itkSmartPointerForwardReference.h"

namespace itk
{

//----------------------------------------------------------------------------
template <class T>
SmartPointerForwardReference<T>
::SmartPointerForwardReference (const SmartPointerForwardReference<T> &p)
{ 
  m_Pointer = p.m_Pointer; 
  this->Register(); 
}
  
//----------------------------------------------------------------------------
template <class T>
SmartPointerForwardReference<T>
::SmartPointerForwardReference (const WeakPointer<T> &p)
{ 
  m_Pointer = p.GetPointer(); 
  this->Register(); 
}
  
//----------------------------------------------------------------------------
template <class T>
SmartPointerForwardReference<T>
::SmartPointerForwardReference (T *p)
{ 
  m_Pointer = p; 
  this->Register(); 
}                             
  
//----------------------------------------------------------------------------
template <class T>
SmartPointerForwardReference<T>
::~SmartPointerForwardReference ()
{
  this->UnRegister();
}
  
//----------------------------------------------------------------------------
template <class T>
T *
SmartPointerForwardReference<T>
::operator -> () const
{ 
  return m_Pointer; 
}

//----------------------------------------------------------------------------
template <class T>
SmartPointerForwardReference<T>
::operator T * () const 
{ 
  return m_Pointer; 
}
  
//----------------------------------------------------------------------------
template <class T>
T *
SmartPointerForwardReference<T>
::GetPointer () const 
{ 
  return m_Pointer; 
}
  
//----------------------------------------------------------------------------
template <class T>
bool 
SmartPointerForwardReference<T>
::operator < (const SmartPointerForwardReference &r)
{ 
  return (void*)m_Pointer < (void*) r.m_Pointer; 
}

//----------------------------------------------------------------------------
template <class T>
bool 
SmartPointerForwardReference<T>
::operator > (const SmartPointerForwardReference &r)
{ 
  return (void*)m_Pointer > (void*) r.m_Pointer; 
}

//----------------------------------------------------------------------------
template <class T>
bool 
SmartPointerForwardReference<T>
::operator <= (const SmartPointerForwardReference &r)
{ 
  return (void*)m_Pointer <= (void*) r.m_Pointer; 
}

//----------------------------------------------------------------------------
template <class T>
bool 
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
SmartPointerForwardReference<T> &
SmartPointerForwardReference<T>
::operator = (const WeakPointer<T> &r)
{ 
  return this->operator = (r.GetPointer()); 
}
  
//----------------------------------------------------------------------------
template <class T>
SmartPointerForwardReference<T> &
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
void 
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
void 
SmartPointerForwardReference<T>
::UnRegister()
{
  if (m_Pointer)
    {
    m_Pointer->UnRegister();
    }
}

} //end namespace

#endif
