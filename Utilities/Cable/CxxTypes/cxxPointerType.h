/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxPointerType.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _cxxPointerType_h
#define _cxxPointerType_h

#include "cxxCvQualifiedType.h"

namespace _cxx_
{


/**
 * Represent a C++ pointer type.
 */
class _cxx_EXPORT PointerType: public Type
{
public:
  typedef PointerType Self;

  virtual RepresentationType GetRepresentationType() const;
  
protected:
  PointerType(const CvQualifiedType&);
  PointerType(const Self&): m_ReferencedType(NULL) {}
  void operator=(const Self&) {}
  virtual ~PointerType() {}
  
private:
  /**
   * The type to which this type refers.
   */
  CvQualifiedType m_ReferencedType;
  
  bool CanConvertFromArrayType(const CvQualifiedType&, bool, bool) const;
  
  friend TypeSystem;
};

} // namespace _cxx_


#endif
