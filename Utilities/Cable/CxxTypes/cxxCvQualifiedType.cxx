/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxCvQualifiedType.cxx
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
 * Default constructor.  This only exists to allow CvQualifiedType instances
 * to be stored in STL containers.
 */
CvQualifiedType::CvQualifiedType():
  m_Type(NULL),
  m_Const(false),
  m_Volatile(false)
{
}


/**
 * Don't call this.  CvQualifiedType instances should be obtained from
 * Type::GetCvQualifiedType().
 * Constructor takes a pointer to the type to which these qualifiers
 * refer.  It defaults all qualifier flags to false.
 */
CvQualifiedType::CvQualifiedType(const Type* in_type):
  m_Type(in_type),
  m_Const(false),
  m_Volatile(false)
{
}


/**
 * Don't call this.  CvQualifiedType instances should be obtained from
 * Type::GetCvQualifiedType().
 * This constructor takes a pointer to the type to which these qualifiers
 * refer, and settings for the const and volatile cv-qualifier flags.
 */
CvQualifiedType::CvQualifiedType(const Type* in_type,
                                 bool in_const, bool in_volatile):
  m_Type(in_type),
  m_Const(in_const),
  m_Volatile(in_volatile)
{
}


/**
 * Copy constructor.
 */
CvQualifiedType::CvQualifiedType(const Self& r):
  m_Type(r.m_Type),
  m_Const(r.m_Const),
  m_Volatile(r.m_Volatile)
{
}


/**
 * Construct a CvQualifiedType with the given cv-qualifiers added.
 */
CvQualifiedType CvQualifiedType::GetMoreQualifiedType(bool isConst,
                                                      bool isVolatile) const
{
  return CvQualifiedType(m_Type, m_Const || isConst, m_Volatile || isVolatile);
}


/**
 * Return whether this CvQualifiedType is equally or more cv-qualified at
 * the top level.
 */
bool
CvQualifiedType
::IsEquallyOrMoreCvQualifiedThan(const CvQualifiedType& cvType) const
{
  return !((cvType.IsConst() && !m_Const)
           || (cvType.IsVolatile() && !m_Volatile));
}


/**
 * Return the type name with its cv-qualifiers.
 */
String CvQualifiedType::GenerateName(const String& indirection,
                                     bool isConst, bool isVolatile) const
{
  bool isc = isConst || m_Const;
  bool isv = isVolatile || m_Volatile;
  
  return m_Type->GenerateName(indirection, isc, isv);
}


/**
 * CvQualifiedTypes compare equal iff they refer to the same Type, and
 * have the same cv-qualifiers.
 */
bool CvQualifiedType::operator== (const Self& r) const
{
  return ((m_Type->Id() == r.m_Type->Id())
          && (m_Const == r.m_Const)
          && (m_Volatile == r.m_Volatile));
}


/**
 * Uniquely orders CvQualifiedType instances.  Useful for using them as
 * map keys.
 */
bool CvQualifiedType::operator< (const Self& r) const
{
  // First, compare the Type pointers.  Here we take advantage of the
  // fact that types are generated uniquely from the TypeSystem's factory,
  // so that the same type always has the same pointer value.
  if(m_Type->Id() < r.m_Type->Id())
    {
    return true;
    }
  else if(m_Type->Id() > r.m_Type->Id())
    {
    return false;
    }
  else // if(m_Type->Id() == r.m_Type->Id())
    {
    // The base type is the same.  Compare the cv-qualifiers.
    int lhs = (int(m_Const << 1) | int(m_Volatile));
    int rhs = (int(r.m_Const << 1) | int(r.m_Volatile));
    return (lhs < rhs);
    }
}


/**
 * Retrieve the kind of Type to which the cv-qualifiers are applied.
 */
RepresentationType CvQualifiedType::GetRepresentationType() const
{
  return m_Type->GetRepresentationType();
}



} // namespace _cxx_
