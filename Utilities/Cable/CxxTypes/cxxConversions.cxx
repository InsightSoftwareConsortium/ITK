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

namespace _cxx_
{

// public functions:

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

