/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxTypedefType.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _cxxTypedefType_h
#define _cxxTypedefType_h

#include "cxxTypes.h"

namespace _cxx_
{

/**
 * Represent a C++ typedef.  It is simply a name for a CvQualifiedType.
 */
class TypedefType: public Type
{
public:
  typedef TypedefType Self;
  
  virtual RepresentationType GetRepresentationType() const;
  virtual bool IsTypedefType() const;
  
  virtual const Type* Id() const;
  virtual CvQualifiedType GetCvQualifiedType(bool, bool) const;
  
protected:
  TypedefType(const CvQualifiedType&);
  TypedefType(const Self&): m_CvQualifiedType(NULL) {}
  void operator=(const Self&) {}
  virtual ~TypedefType() {}
  
private:
  /**
   * The type to which the typedef refers, including optional cv-qualifiers.
   */
  CvQualifiedType m_CvQualifiedType;
};


} // namespace _cxx_

#endif
