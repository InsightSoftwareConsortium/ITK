/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxCvQualifiedType.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _cxxCvQualifiedType_h
#define _cxxCvQualifiedType_h

#include "cxxType.h"

#include <vector>

namespace _cxx_
{


/**
 * Hold cv-qualifiers attached to a type.
 */
class _cxx_EXPORT CvQualifiedType
{
public:
  typedef CvQualifiedType Self;
  
  CvQualifiedType();
  CvQualifiedType(const Type*);
  CvQualifiedType(const Type*, bool, bool);
  CvQualifiedType(const Self&);
  
  bool IsConst() const    { return m_Const; }
  bool IsVolatile() const { return m_Volatile; }
  
  const Type* GetType() const { return m_Type; }
  
  bool operator== (const Self&) const;
  bool operator< (const Self&) const;
  
private:
  /**
   * The type to which these cv-qualifiers apply.
   */
  const Type* m_Type;
  
  /**
   * Flag for presence of "const" cv-qualifier.
   */
  bool m_Const;

  /**
   * Flag for presence of "volatile" cv-qualifier.
   */
  bool m_Volatile;
};

/**
 * A vector of CvQualifiedType instances.
 */
typedef std::vector<CvQualifiedType> CvQualifiedTypes;
  
} // namespace _cxx_


#endif
