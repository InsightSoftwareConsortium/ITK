/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxPointerType.cxx
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
RepresentationType PointerType::GetRepresentationType() const
{
  return PointerType_id;
}


String PointerType::GenerateName(const String& indirection,
                                 bool isConst, bool isVolatile) const
{
  String cv = this->GetRightCvString(isConst, isVolatile);
  String indirect = "*"+cv;
  if(indirection != "")
    {
    indirect += " "+indirection;
    }
  return m_ReferencedType.GenerateName(indirect);
}


/**
 * Constructor takes the cv-qualified type to which the pointer points.
 */
PointerType::PointerType(const CvQualifiedType& in_type):
  m_ReferencedType(in_type)
{
}

} // namespace _cxx_
