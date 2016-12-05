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
#ifndef itkSmartPointerForwardReference_h
#define itkSmartPointerForwardReference_h

#include "itkWeakPointer.h"

#include <iostream>

namespace itk
{
/** \class SmartPointerForwardReference
 * \brief Implements transparent reference counting in situations where forward
 *        references / cyclic include dependencies are a problem.
 *
 * SmartPointerForwardReference implements reference counting by overloading
 * operator -> (and *) among others. This allows natural interface
 * to the class referred to by the pointer without having to invoke
 * special Register()/UnRegister() methods directly.
 *
 * This class is nearly identical to itkSmartPointer except that is used in
 * situations where forward references or cyclic include dependencies become
 * a problem. This class requires that the .h file is included in the .h file
 * of the class using it, and the .hxx file is included in the .cxx/.hxx file
 * of the class using it. (Make sure that SmartPointerForwardReference.hxx is
 * included last in the .cxx/.hxx list of includes.)
 *
 * \ingroup ITKSystemObjects
 * \ingroup DataAccess
 * \ingroup ITKCommon
 */
template< typename T >
class ITK_TEMPLATE_EXPORT SmartPointerForwardReference
{
public:
  /** Constructor  */
  SmartPointerForwardReference ()
  { m_Pointer = ITK_NULLPTR; }

  /** Const constructor  */
  SmartPointerForwardReference (const SmartPointerForwardReference< T > & p);

  /** Construct from a WeakPointer */
  SmartPointerForwardReference (const WeakPointer< T > & p);

  /** Constructor to pointer p  */
  SmartPointerForwardReference (T *p);

  /** Destructor  */
  ~SmartPointerForwardReference ();

  /** Overload operator ->  */
  T * operator->() const;

  /** Return pointer to object.  */
  operator T *() const;

  /** Access function to pointer. */
  T * GetPointer() const;

  /** Comparison of pointers. Less than comparison.  */
  bool operator<(const SmartPointerForwardReference & r);

  /** Comparison of pointers. Greater than comparison.  */
  bool operator>(const SmartPointerForwardReference & r);

  /** Comparison of pointers. Less than or equal to comparison.  */
  bool operator<=(const SmartPointerForwardReference & r);

  /** Comparison of pointers. Greater than or equal to comparison.  */
  bool operator>=(const SmartPointerForwardReference & r);

  /** Overload operator assignment.  */
  SmartPointerForwardReference & operator=(const SmartPointerForwardReference & r);

  /** Overload operator assignment.  */
  SmartPointerForwardReference & operator=(const WeakPointer< T > & r);

  /** Overload operator assignment.  */
  SmartPointerForwardReference & operator=(T *r);

  /** Function to print object pointed to  */
  T * Print(std::ostream & os) const;

private:
  /** The pointer to the object referred to by this smart pointer. */
  T *m_Pointer;

  void Register();

  void UnRegister() ITK_NOEXCEPT;
};

template< typename T >
std::ostream & operator<<(std::ostream & os, SmartPointerForwardReference< T > p)
{
  p.Print(os);
  return os;
}
} // end namespace itk

#endif
