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
    m_Pointer(0),
    m_IsOwner(false)
    { }

  /** Copy constructor.  */
  explicit AutoPointer ( AutoPointer & p )
    {
    m_IsOwner = p.IsOwner(); // Ownership can only be taken from another owner
    m_Pointer = p.ReleaseOwnership(); // release ownership if appropriate
    }


  /** Constructor to pointer p.  */
  explicit AutoPointer ( ObjectType * p, bool takeOwnership ):
                    m_Pointer(p),
                    m_IsOwner(takeOwnership)
      { }


  /** Destructor.  */
  ~AutoPointer ()
    { 
    if( m_IsOwner && m_Pointer ) 
      { 
      delete m_Pointer;
      }
    m_Pointer = 0;
    m_IsOwner = false;
    }
  
  /** Overload operator ->.  */
  ObjectType *operator -> () const
    { return m_Pointer; }

  /** Clear the AutoPointer. If it had a pointer the object
      is deleted and the pointer is set to null. */
  void Reset( void ) 
    {
    if( m_IsOwner && m_Pointer ) 
      { 
      delete m_Pointer;
      }
    m_Pointer = 0;
    m_IsOwner = false;
    }


  /** Set explicitly the Ownership */
  void TakeOwnership(void) 
    { m_IsOwner = true; }

  /** Set explicitly the Ownership */
  void TakeOwnership(ObjectType * objectptr) 
    { 
    if( m_IsOwner && m_Pointer ) 
      { 
      delete m_Pointer; // remove the current one
      }
    m_Pointer = objectptr;
    m_IsOwner = true;
    }

  /** Reject explicitly the Ownership */
  void TakeNoOwnership(ObjectType * objectptr) 
    { 
    if( m_IsOwner && m_Pointer ) 
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
      and return the raw pointer so it can be hold by 
      another AutoPointer. This operation is intended to
      be used for facilitating polymorphism.
  
      Example: if class Cow derives from Mammal,
      AutoPointer<Cow> onecow = new Cow;
      AutoPointer<Mammal> onemammal = onecow.ReleaseOwnership(); 

      Note that the AutoPointer is still pointing to the 
      object after the ReleaseOwnership operation but it doesn't 
      owns the object anymore.

    */
  ObjectType * ReleaseOwnership( void ) 
    {
    m_IsOwner = false;
    return m_Pointer;
    }

  /** Access function to pointer. */
  ObjectType *GetPointer () const 
    { return m_Pointer; }

   /** Comparison of pointers. Equal comparison.  */
  bool operator == (const AutoPointer &r) const
    { return (void*)m_Pointer == (void*) r.m_Pointer; }

  /** Comparison of pointers. NonEqual comparison.  */
  bool operator != (const AutoPointer &r) const
    { return (void*)m_Pointer != (void*) r.m_Pointer; }
  
  /** Comparison of pointers. Less than comparison.  */
  bool operator < (const AutoPointer &r) const
    { return (void*)m_Pointer < (void*) r.m_Pointer; }
  
  /** Comparison of pointers. Greater than comparison.  */
  bool operator > (const AutoPointer &r) const
    { return (void*)m_Pointer > (void*) r.m_Pointer; }

  /** Comparison of pointers. Less than or equal to comparison.  */
  bool operator <= (const AutoPointer &r) const
    { return (void*)m_Pointer <= (void*) r.m_Pointer; }

  /** Comparison of pointers. Greater than or equal to comparison.  */
  bool operator >= (const AutoPointer &r) const
    { return (void*)m_Pointer >= (void*) r.m_Pointer; }

  /** Overload operator assignment.  */
  AutoPointer &operator = (AutoPointer &r) const
    { 
    AutoPointer( r ).Swap( *this );
    return *this;
    }
  
  /** Casting operator to boolean. This is used in conditional 
      statments to check the content of the pointer against null */
  operator bool () const
    { return (m_Pointer!=NULL); }

  /** Function to print object pointed to.  */
  ObjectType *Print (std::ostream& os) const 
    { 
    // This prints the object pointed to by the pointer  
    (*m_Pointer).Print(os);  
    os << "Owner: " << m_IsOwner << std::endl;
    return m_Pointer;
    } 

private:

  /** Exchange the content of two AutoPointers */
  void Swap(AutoPointer &r) throw()
    { 
    ObjectType * temp = m_Pointer;
    m_Pointer         = r.m_Pointer;
    r.m_Pointer       = temp; 
    }
 

  /** The pointer to the object referrred to by this smart pointer. */
  ObjectType* m_Pointer;
  bool        m_IsOwner;
};  

  
template <typename T>
std::ostream& operator<< (std::ostream& os, AutoPointer<T> p) 
{
  p.Print(os); 
  os << "Owner: " << p.IsOwner() << std::endl;
  return os;
}


/** This templated function is intended to facilitate the
    transfer between AutoPointers of Derived class to Base class */
template <typename TAutoPointerBase, typename TAutoPointerDerived>
void
ITK_EXPORT TransferAutoPointer(TAutoPointerBase & pa, TAutoPointerDerived & pb)
{
  pa.TakeNoOwnership( pb.GetPointer() ); // give a chance to natural polymorphism
  if( pb.IsOwner() )
    {
    pa.TakeOwnership();      // pa Take Ownership
    pb.ReleaseOwnership();   // pb Release Ownership and clears
    }
}


} // end namespace itk
  
#endif
