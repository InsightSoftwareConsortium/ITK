/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxArrayType.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "cxxTypes.h"

#include <strstream>

namespace _cxx_
{


/**
 * Retrieve what kind of Type this is.
 */
RepresentationType
ArrayType
::GetRepresentationType() const
{
  return ArrayType_id;
}


String ArrayType::GenerateName(const String& indirection,
                               bool isConst, bool isVolatile) const
{
  std::strstream length;
  length << m_Length;
  String lengthStr = length.str();
  return m_ElementType.GenerateName("", isConst, isVolatile)+"("+indirection+")["+lengthStr+"]";
}


/**
 * Constructor takes the type of the elements in the array, and the length.
 */
ArrayType
::ArrayType(const CvQualifiedType& in_elementType, unsigned long in_length):
  m_ElementType(in_elementType),
  m_Length(in_length)
{
}

} // namespace _cxx_
