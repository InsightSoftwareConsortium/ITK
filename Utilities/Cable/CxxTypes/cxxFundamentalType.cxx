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
RepresentationType
FundamentalType
::GetRepresentationType() const
{
  return FundamentalType_id;
}


/**
 * Test whether this type with the given cv-qualifiers can convert to
 * the given CvQualifiedType.
 */
bool
FundamentalType
::CanConvertTo(const CvQualifiedType&, bool, bool) const
{
  return false;
}


/**
 * Constructor takes the Id of the fundamental type.
 */
FundamentalType::FundamentalType(Id in_id):
  m_Id(in_id)
{
}


} // namespace _cxx_
