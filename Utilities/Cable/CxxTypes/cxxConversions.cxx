/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxConversions.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "cxxConversions.h"

#include <iostream>

namespace _cxx_
{

// public functions:


/**
 * Determine if the given PointerType "from" can conver to the given
 * PointerType "to" according to section 4.4 of the C++ standard.
 */
bool Conversions::IsValidQualificationConversion(const PointerType* from,
                                                 const PointerType* to)
{
  bool allConst = true;
  const Type* t1 = from;
  const Type* t2 = to;
  
  // Follow each PointerType until two identical types are reached.
  while(t1->Id() != t2->Id())
    {
    // Make sure they are not conflicting pointer types.
    if((t1->IsPointerType() && t2->IsPointerType())
       || ((t1->IsPointerToMemberType() && t2->IsPointerToMemberType())
           && (PointerToMemberType::SafeDownCast(t1)->GetClassType()->Id()
               == PointerToMemberType::SafeDownCast(t2)->GetClassType()->Id())))
      {
      // Dereference the pointer types.
      const PointerType* p1 = PointerType::SafeDownCast(t1);
      const PointerType* p2 = PointerType::SafeDownCast(t2);
      CvQualifiedType deRef1 = p1->GetPointedToType();
      CvQualifiedType deRef2 = p2->GetPointedToType();
      t1 = deRef1.GetType();
      t2 = deRef2.GetType();      
      
      // Check for cv-qualifier adjustment correctness.
      // See 4.4/4.
      bool t1_const = deRef1.IsConst();
      bool t1_volatile = deRef1.IsVolatile();
      bool t2_const = deRef2.IsConst();
      bool t2_volatile = deRef2.IsVolatile();
      // If const is in cv_1_j, then const is in cv_2_j, and similarly
      // for volatile.
      if((t1_const && !t2_const) || (t1_volatile && !t2_volatile))
        {
        return false;
        }
      // If cv_1_j and cv_2_j are different, then const is in every
      // cv_2_k for k < j.
      if(((t1_const != t2_const) || (t1_volatile != t2_volatile)) 
         && !allConst)
        {
        return false;
        }
      if(!t2_const)
        {
        allConst = false;
        }
      }
    else
      {
      // Mismatch of Type.  Given types are not similar.
      return false;
      }
    }
  return true;
}


/**
 * Test if an object of type "from" can be bound to a function parameter
 * of type "to".
 *
 * This test is specified in 13.3.3.1 of the C++ Standard.
 */
bool Conversions::CanConvert(const CvQualifiedType& from,
                             const CvQualifiedType& to)
{
  // 13.3.3.1/5
  // For the case where the parameter type is a reference, see 13.3.3.1.4
  if(to.IsReferenceType())
    {
    const ReferenceType* toReferenceType = dynamic_cast<const ReferenceType*>(to.GetType());
    return ReferenceBinding(from, toReferenceType->GetReferencedType());
    }

  // 13.3.3.1/6
  // When the parameter type is not a reference, the implicit conversion
  // sequence models a copy-initialization of the parameter from the
  // argument expression.
  
  // --- hacked below here --- reimplement later
  
  // If the arguments are identical, we are done.
  if(to == from)
    {
    return true;
    }
  
#if 0
  // Branch based on the target type.
  switch (to.GetTypeRepresentation())
    {
    case ArrayType_id:           return false;
    case ClassType_id:           return false;
    case PointerType_id:         return false;
    case PointerToMemberType_id: return false;
    case FunctionType_id:        return false;
    case FundamentalType_id:
      return false;
    case Undefined_id:
    default:
      return false;
   }
#endif
  return false;
}

// private helpers:
  
/**
 * Determine if an object of type "from" can be bound to a reference to
 * the given type "to".
 *
 * This test is specified in 13.3.3.1.4 of the C++ Standard.
 */
bool Conversions::ReferenceBinding(const CvQualifiedType& from,
                                   const CvQualifiedType& to)
{
  // See if there is a direct binding.
  // 8.5.3/4
  return to == from;
}

  
} // namespace _cxx_

