/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxReferenceType.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _cxxReferenceType_h
#define _cxxReferenceType_h

#include "cxxCvQualifiedType.h"

namespace _cxx_
{


/**
 * Represents a C++ reference type.
 */
class ReferenceType: public Type
{
public:
  typedef ReferenceType Self;
  
  virtual RepresentationType GetRepresentationType() const;
  
protected:
  virtual bool CanConvertTo(const CvQualifiedType&, bool, bool) const;
  
  ReferenceType(const CvQualifiedType&);
  ReferenceType(const Self&): m_ReferencedType(NULL) {}
  void operator=(const Self&) {}
  virtual ~ReferenceType() {}
  
private:
  /**
   * The type to which this type refers.
   */
  CvQualifiedType m_ReferencedType;
  
  friend TypeSystem;
};


} // namespace _cxx_


#endif
