/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxClassType.cxx
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
RepresentationType ClassType::GetRepresentationType() const
{
  return ClassType_id;
}


/**
 * Try to cast the given Type to an ClassType.  If this returns, the
 * pointer will be valid.  If the cast is not allowed, an exception is
 * thrown.
 */
ClassType* ClassType::SafeDownCast(Type* t)
{
  ClassType* result = dynamic_cast<ClassType*>(t);
  if(!result) { throw TypeDownCastException(t, ClassType_id); }
  return result;
}


/**
 * Try to cast the given Type to an ClassType.  If this returns, the
 * pointer will be valid.  If the cast is not allowed, an exception is
 * thrown.
 */
const ClassType* ClassType::SafeDownCast(const Type* t)
{
  const ClassType* result = dynamic_cast<const ClassType*>(t);
  if(!result) { throw TypeDownCastException(t, ClassType_id); }
  return result;
}


/**
 * Get the name of the class.
 */
String ClassType::GetName() const
{
  return m_Name;
}


String ClassType::GenerateName(const String& indirection,
                               bool isConst, bool isVolatile) const
{
  String cv = this->GetLeftCvString(isConst, isVolatile);
  return cv+m_Name+indirection;
}


/**
 * Constructor for ClassType just takes the name of the class.  This name
 * may include template parameters of an instantiation.
 */
ClassType::ClassType(const String& name,
                     const ClassTypes& parents):
  m_Name(name),
  m_Parents(parents)
{
}


} // namespace _cxx_
