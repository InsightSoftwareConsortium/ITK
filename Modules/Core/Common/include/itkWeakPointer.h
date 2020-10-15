/*=========================================================================
 *
 *  Copyright NumFOCUS
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
#ifndef itkWeakPointer_h
#define itkWeakPointer_h

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
 * \ingroup ITKCommon
 */
template <typename TObjectType>
class WeakPointer
{
public:
  /** Extract information from template parameter. */
  using ObjectType = TObjectType;

  /** Explicitly-defaulted default-constructor.
   * \note The other five "special member functions" (copy-constructor,
   * copy-assignment operator, move-constructor, move-assignment operator,
   * and destructor) are defaulted implicitly, following the C++ "Rule of Zero".
   */
  WeakPointer() = default;

  /** Constructor, converting from `nullptr`.  */
  WeakPointer(std::nullptr_t) {}

  /** Constructor to pointer p.  */
  WeakPointer(ObjectType * p)
    : m_Pointer(p)
  {}

  /** Overload operator ->.  */
  ObjectType * operator->() const { return m_Pointer; }

  /** Return pointer to object.  */
  operator ObjectType *() const { return m_Pointer; }

  /** Template comparison operators. */
  template <typename R>
  bool
  operator==(R r) const
  {
    return (m_Pointer == (ObjectType *)r);
  }

  template <typename R>
  bool
  operator!=(R r) const
  {
    return (m_Pointer != (ObjectType *)r);
  }

  /** Access function to pointer. */
  ObjectType *
  GetPointer() const
  {
    return m_Pointer;
  }

  /** Test if the pointer is not NULL. */
  bool
  IsNotNull() const
  {
    return m_Pointer != nullptr;
  }

  /** Test if the pointer is NULL. */
  bool
  IsNull() const
  {
    return m_Pointer == nullptr;
  }

  /** Comparison of pointers. Less than comparison.  */
  bool
  operator<(const WeakPointer & r) const
  {
    return (void *)m_Pointer < (void *)r.m_Pointer;
  }

  /** Comparison of pointers. Greater than comparison.  */
  bool
  operator>(const WeakPointer & r) const
  {
    return (void *)m_Pointer > (void *)r.m_Pointer;
  }

  /** Comparison of pointers. Less than or equal to comparison.  */
  bool
  operator<=(const WeakPointer & r) const
  {
    return (void *)m_Pointer <= (void *)r.m_Pointer;
  }

  /** Comparison of pointers. Greater than or equal to comparison.  */
  bool
  operator>=(const WeakPointer & r) const
  {
    return (void *)m_Pointer >= (void *)r.m_Pointer;
  }

  /** Function to print object pointed to.  */
  ObjectType *
  Print(std::ostream & os) const
  {
    if (this->IsNull())
    {
      os << "(null)";
    }
    else
    {
      // This prints the object pointed to by the pointer
      (*m_Pointer).Print(os);
    }
    return m_Pointer;
  }

private:
  /** The pointer to the object referred to by this smart pointer. */
  ObjectType * m_Pointer{ nullptr };
};

template <typename T>
std::ostream &
operator<<(std::ostream & os, const WeakPointer<T> p)
{
  p.Print(os);
  return os;
}
} // end namespace itk

#endif
