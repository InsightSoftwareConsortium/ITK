/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit (ITK)
  Module:    itkSmartPointerForwardReference.h
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
#ifndef __itkSmartPointerForwardReference_h
#define __itkSmartPointerForwardReference_h

#include "itkMacro.h"
#include <iostream>

namespace itk
{

/** \class SmartPointerForwardReference
 * \brief Implements transparent reference counting in situations where forward
 *        references / cyclic include depndencies are a problem.
 *
 * SmartPointerForwardReference implements reference counting by overloading
 * operator -> (and *) among others. This allows natural interface
 * to the class referred to by the pointer without having to invoke
 * special Register()/UnRegister() methods directly.
 *
 * This class is nearly identical to itkSmartPointer except that is used in
 * situations where forward references or cyclic include dependencies become
 * a problem. This class requires that the .h file is included in the .h file
 * of the class using it, and the .txx file is included in the .cxx/.txx file
 * of the class using it. (Make sure that SmartPointerForwardReference.txx is
 * included last in the .cxx/.txx list of includes.)
 */
template <class T>
class ITK_EXPORT SmartPointerForwardReference 
{
public:
  /** 
   * Constructor 
   */
  SmartPointerForwardReference () 
    {m_Pointer = 0;}

  /** 
   * Const constructor 
   */
  SmartPointerForwardReference (const SmartPointerForwardReference<T> &p);
  
  /** 
   * Construct from a WeakPointer
   */
  SmartPointerForwardReference (const WeakPointer<T> &p);
  
  /** 
   * Constructor to pointer p 
   */
  SmartPointerForwardReference (T *p);
  
  /** 
   * Destructor 
   */
  ~SmartPointerForwardReference ();
  
  /** 
   * Overload operator -> 
   */
  T *operator -> () const;

  /** 
    * Return pointer to object.
    */
  operator T * () const;
  
  /** 
   * Access function to pointer.
   */
  T *GetPointer () const;
  
  /** 
   * Comparison of pointers. Less than comparison. 
   */
  bool operator < (const SmartPointerForwardReference &r);

  /** 
   * Comparison of pointers. Greater than comparison. 
   */
  bool operator > (const SmartPointerForwardReference &r);

  /** 
   * Comparison of pointers. Less than or equal to comparison. 
   */
  bool operator <= (const SmartPointerForwardReference &r);

  /** 
   * Comparison of pointers. Greater than or equal to comparison. 
   */
  bool operator >= (const SmartPointerForwardReference &r);

  /** 
   * Overload operator assignment. 
   */
  SmartPointerForwardReference &operator = (const SmartPointerForwardReference &r);
  
  /** 
   * Overload operator assignment. 
   */
  SmartPointerForwardReference &operator = (const WeakPointer<T> &r);
  
  /** 
   * Overload operator assignment. 
   */
  SmartPointerForwardReference &operator = (T *r);
  
  /** 
   * Function to print object pointed to 
   */
  T *Print (std::ostream& os) const;

private:
  /* The pointer to the object referrred to by this smart pointer. */
  T* m_Pointer;

  void Register();
  void UnRegister();
  
};
  
  template <typename T>
  std::ostream& operator<< (std::ostream& os, SmartPointerForwardReference<T> p) 
  {
  p.Print(os); 
  return os;
  }

} // end namespace itk
  
#endif
