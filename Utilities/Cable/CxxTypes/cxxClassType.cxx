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
RepresentationType
ClassType
::GetRepresentationType() const
{
  return ClassType_id;
}


/**
 * Add a public superclass to this class.
 */
void
ClassType
::AddParent(const ClassType* p)
{
  m_Parents.push_back(p);
}


/**
 * Constructor for ClassType just takes the name of the class.  This name
 * may include template parameters of an instantiation.
 */
ClassType
::ClassType(const String& in_name):
  m_Name(in_name)
{
}


} // namespace _cxx_
