/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxType.cxx
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
 * Test whether this is a TypedefType.  This is a special test because
 * TypedefType's GetRepresentationType() passes the call through to the
 * real type.  This call is not passed through.
 */
bool Type::IsTypedefType() const
{
  return false;
}


/**
 * Return the pointer that is used to identify this type.
 * For all non-typedef types, the "this" pointer is the correct value.
 */
const Type* Type::Id() const
{
  return this;
}
  

/**
 * Given cv-qualifiers, construct the CvQualifiedType referring to
 * this type.  The result may be more cv-qualified than that given if
 * this is a typedef type.
 */
CvQualifiedType
Type::GetCvQualifiedType(bool isConst, bool isVolatile) const
{
  // This is not a TypedefType.  Just construct the qualified form and
  // return it.
  return CvQualifiedType(this, isConst, isVolatile);
}


} // namespace _cxx_
