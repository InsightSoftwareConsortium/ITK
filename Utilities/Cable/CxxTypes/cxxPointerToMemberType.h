/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxPointerToMemberType.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _cxxPointerToMemberType_h
#define _cxxPointerToMemberType_h

#include "cxxPointerType.h"

namespace _cxx_
{


/**
 * Represents a C++ pointer-to-member type.
 */
class _cxx_EXPORT PointerToMemberType: public PointerType
{
public:
  typedef PointerToMemberType Self;
  
  virtual RepresentationType GetRepresentationType() const;
  virtual String GenerateName(const String& indirection,
                              bool isConst, bool isVolatile) const;

protected:
  PointerToMemberType(const CvQualifiedType&, const ClassType*);  
  PointerToMemberType(const Self& s): PointerType(s), m_ClassType(NULL) {}
  void operator=(const Self&) {}
  virtual ~PointerToMemberType() {}
  
private:
  /**
   * The class type holding the member.
   */
  const ClassType* m_ClassType;
  
  friend TypeSystem;
};

} // namespace _cxx_


#endif
