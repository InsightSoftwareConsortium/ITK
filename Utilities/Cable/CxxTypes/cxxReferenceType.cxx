/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxReferenceType.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "cxxTypes.h"

namespace _cxx_
{


/**
 * Retrieve what kind of Type this is.
 */
RepresentationType ReferenceType::GetRepresentationType() const
{
  return ReferenceType_id;
}


/**
 * Try to cast the given Type to an ReferenceType.  If this returns, the
 * pointer will be valid.  If the cast is not allowed, an exception is
 * thrown.
 */
ReferenceType* ReferenceType::SafeDownCast(Type* t)
{
  ReferenceType* result = dynamic_cast<ReferenceType*>(t);
  if(!result) { throw TypeDownCastException(t, ReferenceType_id); }
  return result;
}


/**
 * Try to cast the given Type to an ReferenceType.  If this returns, the
 * pointer will be valid.  If the cast is not allowed, an exception is
 * thrown.
 */
const ReferenceType* ReferenceType::SafeDownCast(const Type* t)
{
  const ReferenceType* result = dynamic_cast<const ReferenceType*>(t);
  if(!result) { throw TypeDownCastException(t, ReferenceType_id); }
  return result;
}


// Can't have indirection or cv qualifiers.
String ReferenceType::GenerateName(const String&, bool, bool) const
{
  return m_ReferencedType.GenerateName("&");
}


/**
 * Get the CvQualifiedType referenced by this ReferenceType.
 */
const CvQualifiedType& ReferenceType::GetReferencedType() const
{
  return m_ReferencedType;
}


/**
 * Constructor takes the cv-qualified type that is referenced.
 */
ReferenceType::ReferenceType(const CvQualifiedType& in_type):
  m_ReferencedType(in_type)
{
}


} // namespace _cxx_
