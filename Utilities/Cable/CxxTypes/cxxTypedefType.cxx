/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxTypedefType.cxx
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
 * This is a special version for the TypedefType, because it passes the
 * call through to the underlying type.
 */
RepresentationType
TypedefType
::GetRepresentationType() const
{
  return m_CvQualifiedType.GetType()->GetRepresentationType();
}


/**
 * Test whether this is a TypedefType.  This is a special test because
 * TypedefType's GetRepresentationType() passes the call through to the
 * real type.  This call is not passed through.
 */
bool
TypedefType
::IsTypedefType() const
{
  return true;
}


/**
 * Given cv-qualifiers, construct the CvQualifiedType referring to
 * this type.  The result may be more cv-qualified than that given if
 * this is a typedef type.
 */
CvQualifiedType
TypedefType
::GetCvQualifiedType(bool isConst, bool isVolatile) const
{
  // This is a TypedefType, but it may refer to a TypedefType as well.
  // Ask the referred-to-type to construct the CvQualifiedType, but with
  // the addition of all qualifiers specified by this typedef.
  return m_CvQualifiedType.GetType()
    ->GetCvQualifiedType(isConst || m_CvQualifiedType.IsConst(),
                         isVolatile || m_CvQualifiedType.IsVolatile());
}


/**
 * Constructor takes the cv-qualified type that is referenced.
 */
TypedefType
::TypedefType(const CvQualifiedType& in_type):
  m_CvQualifiedType(in_type)
{
}
  


} // namespace _cxx_
