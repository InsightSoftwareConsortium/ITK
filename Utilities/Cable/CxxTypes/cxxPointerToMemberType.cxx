/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxPointerToMemberType.cxx
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
RepresentationType PointerToMemberType::GetRepresentationType() const
{
  return PointerToMemberType_id;
}


String PointerToMemberType::GenerateName(const String& indirection,
                                         bool isConst, bool isVolatile) const
{
  String cv = this->GetRightCvString(isConst, isVolatile);
  String indirect = indirection;
  if(indirect != "")
    {
    indirect += " ";
    }
  indirect += m_ClassType->GetName() + "::*" + cv;
  return m_ReferencedType.GenerateName(indirect);
}


/**
 * Constructor takes cv-qualified type of member, and the type of
 * the class in which the member resides.
 */
PointerToMemberType::PointerToMemberType(const CvQualifiedType& in_type,
                                         const ClassType* in_class):
  PointerType(in_type),
  m_ClassType(in_class)
{
}
  

} // namespace _cxx_
