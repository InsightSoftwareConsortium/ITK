/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxFundamentalType.cxx
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
RepresentationType FundamentalType::GetRepresentationType() const
{
  return FundamentalType_id;
}


/**
 * Try to cast the given Type to an FundamentalType.  If this returns, the
 * pointer will be valid.  If the cast is not allowed, an exception is
 * thrown.
 */
FundamentalType* FundamentalType::SafeDownCast(Type* t)
{
  FundamentalType* result = dynamic_cast<FundamentalType*>(t);
  if(!result) { throw TypeDownCastException(t, FundamentalType_id); }
  return result;
}


/**
 * Try to cast the given Type to an FundamentalType.  If this returns, the
 * pointer will be valid.  If the cast is not allowed, an exception is
 * thrown.
 */
const FundamentalType* FundamentalType::SafeDownCast(const Type* t)
{
  const FundamentalType* result = dynamic_cast<const FundamentalType*>(t);
  if(!result) { throw TypeDownCastException(t, FundamentalType_id); }
  return result;
}


String FundamentalType::GenerateName(const String& indirection,
                                     bool isConst, bool isVolatile) const
{
  String cv = this->GetLeftCvString(isConst, isVolatile);
  return cv+fundamentalTypeNames[m_Id]+indirection;
}


/**
 * Constructor takes the Id of the fundamental type.
 */
FundamentalType::FundamentalType(Id in_id):
  m_Id(in_id)
{
}

const char* FundamentalType::fundamentalTypeNames[NumberOfTypes] =
{
  "unsigned char", "unsigned short", "unsigned int", "unsigned long",
  "signed char", "char", "short", "int", "long", "wchar_t", "bool",
  "float", "double", "long double", "void"
};

} // namespace _cxx_
