/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxFunctionType.cxx
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
RepresentationType FunctionType::GetRepresentationType() const
{
  return FunctionType_id;
}


/**
 * Try to cast the given Type to an FunctionType.  If this returns, the
 * pointer will be valid.  If the cast is not allowed, an exception is
 * thrown.
 */
FunctionType* FunctionType::SafeDownCast(Type* t)
{
  FunctionType* result = dynamic_cast<FunctionType*>(t);
  if(!result) { throw TypeDownCastException(t, FunctionType_id); }
  return result;
}


/**
 * Try to cast the given Type to an FunctionType.  If this returns, the
 * pointer will be valid.  If the cast is not allowed, an exception is
 * thrown.
 */
const FunctionType* FunctionType::SafeDownCast(const Type* t)
{
  const FunctionType* result = dynamic_cast<const FunctionType*>(t);
  if(!result) { throw TypeDownCastException(t, FunctionType_id); }
  return result;
}


String FunctionType::GenerateName(const String& indirection,
                                  bool isConst, bool) const
{
  String returns = m_ReturnType.GetName();
  
  String arguments = "";
  CvQualifiedTypes::const_iterator arg = m_Arguments.begin();
  if(arg != m_Arguments.end())
    {    
    arguments += arg->GetName();
    for(;arg != m_Arguments.end(); ++arg)
      {
      arguments += ", "+arg->GetName();
      }
    }
  String cv = this->GetRightCvString(isConst, false);
  return returns + "(" + indirection + ")( " + arguments + " )" + cv;
}

  
/**
 * Constructor takes the return type of the function.
 */
FunctionType::FunctionType(const CvQualifiedType& returnType,
                           const CvQualifiedTypes& arguments):
  m_ReturnType(returnType),
  m_Arguments(arguments)
{
}


} // namespace _cxx_
