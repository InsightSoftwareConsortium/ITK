/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxArrayType.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _cxxArrayType_h
#define _cxxArrayType_h

#include "cxxCvQualifiedType.h"

namespace _cxx_
{


/**
 * Represents a C-style array type.
 */
class _cxx_EXPORT ArrayType: public Type
{
public:
  typedef ArrayType Self;
  
  virtual RepresentationType GetRepresentationType() const;
  
  const CvQualifiedType& GetElementType() const
    { return m_ElementType; }
  
protected:
  ArrayType(const CvQualifiedType&, unsigned long);
  ArrayType(const Self&): m_ElementType(NULL), m_Length(0) {}
  void operator=(const Self&) {}
  virtual ~ArrayType() {}
  
private:
  /**
   * The type of the array's elements.
   */
  CvQualifiedType m_ElementType;

  /**
   * The length of the array.
   */
  unsigned long m_Length;
  
  friend TypeSystem;
};

} // namespace _cxx_


#endif
