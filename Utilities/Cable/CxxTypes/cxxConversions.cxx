/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxConversions.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "cxxConversions.h"

namespace _cxx_
{

/**
 * Test if an object of type "from" can be bound to a function parameter
 * of type "to".
 *
 * This test mirrors that specified in 13.3.3.1 of the C++ Standard.
 */
bool Conversions::CanConvert(const CvQualifiedType& from,
                             const CvQualifiedType& to) const
{
  // 13.3.3.1/5
  // For the case where the parameter type is a reference, see 13.3.3.1.4
  if(to.GetType()->IsReferenceType())
    {    
    }
}

} // namespace _cxx_
