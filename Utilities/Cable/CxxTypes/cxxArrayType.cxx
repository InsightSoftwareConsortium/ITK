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


/**
 * Try to cast the given Type to an ArrayType.  If this returns, the
 * pointer will be valid.  If the cast is not allowed, an exception is
 * thrown.
 */
ArrayType* ArrayType::SafeDownCast(Type* t)
{
  ArrayType* result = dynamic_cast<ArrayType*>(t);
  if(!result) { throw TypeDownCastException(t, ArrayType_id); }
  return result;
}


/**
 * Try to cast the given Type to an ArrayType.  If this returns, the
 * pointer will be valid.  If the cast is not allowed, an exception is
 * thrown.
 */
const ArrayType* ArrayType::SafeDownCast(const Type* t)
{
  const ArrayType* result = dynamic_cast<const ArrayType*>(t);
  if(!result) { throw TypeDownCastException(t, ArrayType_id); }
  return result;
}


String ArrayType::GenerateName(const String& indirection,
                               bool isConst, bool isVolatile) const
{
  std::strstream length;
  length << m_Length << std::ends;
  String lengthStr = length.str();
  String indirect = "";
  if(indirection.length() > 0)
    {
    indirect = "("+indirection+")";
    }
  return m_ElementType.GenerateName("", isConst, isVolatile)+indirection+"["+lengthStr+"]";
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
