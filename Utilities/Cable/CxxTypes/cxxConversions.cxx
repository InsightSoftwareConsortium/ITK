/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxConversions.cxx
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
#include "cxxConversions.h"

namespace _cxx_
{

// public functions:

/**
 * Determine whether the given ReferenceType "to" can bind to the given
 * type "from" as an identity.  That is, without any conversion other than
 * cv-qualifier adjustment.  Not even a derived-to-base conversion.
 */
bool Conversions::ReferenceCanBindAsIdentity(const CvQualifiedType& from,
                                             const ReferenceType* to)
{
  CvQualifiedType toType = to->GetReferencedType();
  // See if the reference can bind directly to an object.
  if((toType == from)
     || (toType == from.GetMoreQualifiedType(true, false))
     || (toType == from.GetMoreQualifiedType(false, true))
     || (toType == from.GetMoreQualifiedType(true, true)))
    {
    return true;
    }
  if(from.GetType()->IsReferenceType())
    {
    CvQualifiedType fromType = ReferenceType::SafeDownCast(from.GetType())->GetReferencedType();
    if((toType == fromType)
       || (toType == fromType.GetMoreQualifiedType(true, false))
       || (toType == fromType.GetMoreQualifiedType(false, true))
       || (toType == fromType.GetMoreQualifiedType(true, true)))
      {
      return true;
      }
    }
 
  return false;
}

  
/**
 * Determine whether the given ReferenceType "to" can bind to the given
 * type "from" with only a derived-to-base conversion and (possibly)
 * cv-qualifier adjustment.
 */
bool Conversions::ReferenceCanBindAsDerivedToBase(const CvQualifiedType& from,
                                                  const ReferenceType* to)
{
  CvQualifiedType toCvType = to->GetReferencedType();
  
  // Only a ClassType can have a derived-to-base reference binding.
  if(!(toCvType.GetType()->IsClassType() && from.GetType()->IsClassType()))
    {
    return false;
    }
  
  // Make sure cv-qualifiers match up.
  if(!toCvType.IsEquallyOrMoreCvQualifiedThan(from))
    {
    return false;
    }
  
  // Get the ClassType information.
  const ClassType* toType = ClassType::SafeDownCast(toCvType.GetType());
  const ClassType* fromType = ClassType::SafeDownCast(from.GetType());
  
  // See if "fromType" is a subclass of "toType".
  return fromType->IsSubclassOf(toType);
}


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
  while(!Type::Equal(t1, t2))
    {
    // Make sure they are not conflicting pointer types.
    if((t1->IsPointerType() && t2->IsPointerType())
       || ((t1->IsPointerToMemberType() && t2->IsPointerToMemberType())
           && Type::Equal(PointerToMemberType::SafeDownCast(t1)->GetClassType(),
                          PointerToMemberType::SafeDownCast(t2)->GetClassType())))
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

