/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cableReferenceCount.h
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
#ifndef _cableReferenceCount_h
#define _cableReferenceCount_h

#include "cableWin32Header.h"

class Object;

/**
 * Define a smart pointer used for the reference counted Object class below.
 */
template <class T>
class SmartPointer
{
public:
  SmartPointer(): m_Pointer(0) {}
  SmartPointer(const SmartPointer& p): m_Pointer(p.m_Pointer)
    {
      this->Register();
    }

  SmartPointer(T* p): m_Pointer(p)
    {
      this->Register();
    }

  ~SmartPointer()
    {
      this->Unregister();
    }

  /**
   * Allow equality comparison with another pointer.
   */
  bool operator == (SmartPointer r) const { return (m_Pointer==r.m_Pointer); }

  /**
   * Allow equality comparison with another pointer.
   */
  bool operator == (T* r) const { return (m_Pointer==r); }
  
  /**
   * Allow inequality comparison with another pointer.
   */
  bool operator != (SmartPointer r) const { return (m_Pointer!=r.m_Pointer); }

  /**
   * Allow inequality comparison with another pointer.
   */
  bool operator != (T* r) const { return (m_Pointer!=r); }
  
  /**
   * Allow assignment.
   */
  SmartPointer& operator = (const SmartPointer& r)
    {
      if(m_Pointer != r.m_Pointer)
        {
        this->Unregister();
        m_Pointer = r.m_Pointer;
        this->Register();
        }
      return *this;
    }  

  /**
   * Allow assignment.  Conversion operator below allows this to take
   * a normal pointer, or another smart pointer to T or a descendent of T.
   */
  SmartPointer& operator = (T* r)
    {
      if(m_Pointer != r)
        {
        this->Unregister();
        m_Pointer = r;
        this->Register();
        }
      return *this;
    }  

  /**
   * Allow access to members of T as if this were a normal pointer.
   */
  T* operator -> () const
    { 
    return m_Pointer; 
    }

  /**
   * Let smart pointer look like normal pointer to T.
   */
  operator T* () const
    {
      return m_Pointer;
    }

  /**
   * Allow read access to real pointer to make casting easier.
   */
  T* RealPointer(void) const
    {
      return m_Pointer;
    }

private:

  /**
   * Increment reference count of object.
   */
  void Register(void)
    {
      if(m_Pointer)
        {
        ((Object*)m_Pointer)->Register();
        }
    }
  
  /**
   * Decrement reference count of object.
   */
  void Unregister(void)
    {
      if(m_Pointer)
        {
        ((Object*)m_Pointer)->Unregister();
        }
    }
  
  T* m_Pointer;
};


/**
 * Define a reference-counted object type.
 * All classes derived from this must have protected constructors,
 * destructors, and assignment operators.  They must be created with
 * a static New() method.
 */
class PARSERS_EXPORT Object
{
public:
  typedef Object                    Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  /**
   * Create a new reference counted simple object.
   */
  static Pointer New()
    {
      return new Self;
    }
  
  /**
   * Increment object's reference count.
   */
  void Register(void)
    {
      ++m_ReferenceCount;
      if(m_ReferenceCount < 0)
        {
        delete this;
        }
    }
  
  /**
   * Decrement object's reference count.
   */
  void Unregister(void)
    {
      --m_ReferenceCount;
      if(m_ReferenceCount < 0)
        {
        delete this;
        }
    }

protected:
  Object(): m_ReferenceCount(0) {}
  Object(const Self&) {}
  void operator=(const Self&) {}
  virtual ~Object() {}
  
private:
  int m_ReferenceCount;
};

#endif
