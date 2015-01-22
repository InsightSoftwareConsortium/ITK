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
#ifndef itkAutoPointer_h
#define itkAutoPointer_h

#include "itkMacro.h"
#include <iostream>

namespace itk
{
/** \class AutoPointer
 * \brief Implements an Automatic Pointer to an object.
 *
 * AutoPointer is intended to facilitate the construction of
 * objects on the fly for those objects that are not to be shared.
 * An AutoPointer destroys its object when it goes out of scope.
 * Ownership of the object is transferred from one AutoPointer
 * to another AutoPointer when the assignement operator is used.
 * An AutoPointer can release ownership of the object it holds.
 *
 * This class follows the design of the std::auto_ptr class. The main
 * reason for not using the std version is to avoid templated methods,
 * which greatly increase the difficulty of wrapping for Tcl, Python
 * and Java.
 *
 * \ingroup ITKSystemObjects
 * \ingroup DataAccess
 * \ingroup ITKCommon
 */
template< typename TObjectType >
class AutoPointer
{
public:
  /** Extract information from template parameter. */
  typedef TObjectType ObjectType;
  typedef AutoPointer Self;

  /** Constructor.  */
  AutoPointer ():m_Pointer(ITK_NULLPTR), m_IsOwner(false)
  {}

  /** Copy constructor.  */
  explicit AutoPointer (AutoPointer & p)
  {
    m_IsOwner = p.IsOwner();          // Ownership can only be taken from
                                      // another owner
    m_Pointer = p.ReleaseOwnership(); // release ownership if appropriate
  }

  /** Constructor to pointer p.  */
  explicit AutoPointer (ObjectType *p, bool takeOwnership):
    m_Pointer(p), m_IsOwner(takeOwnership)
  {}

  /** Destructor.  */
  ~AutoPointer ()
  {
    this->Reset();
  }

  /** Overload operator ->.  */
  ObjectType * operator->() const
  { return m_Pointer; }

  /** Clear the AutoPointer. If it had a pointer the object
      is deleted and the pointer is set to null. */
  void Reset(void)
  {
    if ( m_IsOwner )
      {
      delete m_Pointer;
      }
    m_Pointer = ITK_NULLPTR;
    m_IsOwner = false;
  }

  /** Explicitly set the ownership */
  void TakeOwnership(void)
  { m_IsOwner = true; }

  /** Explicitly set the ownership */
  void TakeOwnership(ObjectType *objectptr)
  {
    if ( m_IsOwner )
      {
      delete m_Pointer; // remove the current one
      }
    m_Pointer = objectptr;
    m_IsOwner = true;
  }

  /** Explicitly reject ownership */
  void TakeNoOwnership(ObjectType *objectptr)
  {
    if ( m_IsOwner )
      {
      delete m_Pointer; // remove the current one
      }
    m_Pointer = objectptr;
    m_IsOwner = false;
  }

  /** Query for the ownership */
  bool IsOwner(void) const
  { return m_IsOwner; }

  /** Release the pointer hold by the current AutoPointer
   *  and return the raw pointer so it can be hold by
   *  another AutoPointer. This operation is intended to
   *  be used for facilitating polymorphism.
   *
   *  Example: if class Cow derives from Mammal,
   *  AutoPointer<Cow> onecow = new Cow;
   *  AutoPointer<Mammal> onemammal = onecow.ReleaseOwnership();
   *
   *  Note that the AutoPointer still points to the object after the
   *  ReleaseOwnership operation, but it doesn't own the object any
   *  more. */
  ObjectType * ReleaseOwnership(void)
  {
    m_IsOwner = false;
    return m_Pointer;
  }

  /** Access function to pointer. */
  ObjectType * GetPointer() const
  { return m_Pointer; }

  /** Comparison of pointers. Equal comparison.  */
  bool operator==(const AutoPointer & r) const
  { return (void *)m_Pointer == (void *)r.m_Pointer; }

  /** Comparison of pointers. NonEqual comparison.  */
  bool operator!=(const AutoPointer & r) const
  { return (void *)m_Pointer != (void *)r.m_Pointer; }

  /** Comparison of pointers. Less than comparison.  */
  bool operator<(const AutoPointer & r) const
  { return (void *)m_Pointer < (void *)r.m_Pointer; }

  /** Comparison of pointers. Greater than comparison.  */
  bool operator>(const AutoPointer & r) const
  { return (void *)m_Pointer > (void *)r.m_Pointer; }

  /** Comparison of pointers. Less than or equal to comparison.  */
  bool operator<=(const AutoPointer & r) const
  { return (void *)m_Pointer <= (void *)r.m_Pointer; }

  /** Comparison of pointers. Greater than or equal to comparison.  */
  bool operator>=(const AutoPointer & r) const
  { return (void *)m_Pointer >= (void *)r.m_Pointer; }

  /** Overload operator assignment.  */
  AutoPointer & operator=(AutoPointer & r)
  {
    AutoPointer(r).Swap(*this);
    return *this;
  }

  /** Casting operator to boolean. This is used in conditional
      statments to check the content of the pointer against null */
  operator bool() const
                { return ( m_Pointer != ITK_NULLPTR ); }

  /** Function to print object pointed to.  */
/*  ObjectType *Print (std::ostream& os) const
    {
    // This prints the object pointed to by the pointer
    (*m_Pointer).Print(os);
    os << "Owner: " << m_IsOwner << std::endl;
    return m_Pointer;
    }
*/

private:

  /** Exchange the content of two AutoPointers */
  void Swap(AutoPointer & r)
  ITK_NOEXCEPT
  {
    ObjectType *temp = m_Pointer;

    m_Pointer         = r.m_Pointer;
    r.m_Pointer       = temp;
  }

  /** The pointer to the object referred to by this smart pointer. */
  ObjectType *m_Pointer;
  bool        m_IsOwner;
};

template< typename T >
std::ostream & operator<<(std::ostream & os, AutoPointer< T > p)
{
  p.Print(os);
  os << "Owner: " << p.IsOwner() << std::endl;
  return os;
}

/** This templated function is intended to facilitate the
    transfer between AutoPointers of Derived class to Base class */
template< typename TAutoPointerBase, typename TAutoPointerDerived >
void
 TransferAutoPointer(TAutoPointerBase & pa, TAutoPointerDerived & pb)
{
  // give a chance to natural polymorphism
  pa.TakeNoOwnership( pb.GetPointer() );
  if ( pb.IsOwner() )
    {
    pa.TakeOwnership();      // pa Take Ownership
    pb.ReleaseOwnership();   // pb Release Ownership and clears
    }
}
} // end namespace itk

#endif
