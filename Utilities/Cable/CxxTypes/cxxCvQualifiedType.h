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
  
  String GetName() const { return this->GenerateName(""); }
  const Type* GetType() const { return m_Type; }
  CvQualifiedType GetMoreQualifiedType(bool isConst, bool isVolatile) const;
  String GenerateName(const String& indirection,
                      bool isConst = false, bool isVolatile = false) const;
  
  bool operator== (const Self&) const;
  bool operator< (const Self&) const;
  
  RepresentationType GetRepresentationType() const;
  
  /*@{
   * Quick type representation test.
   */     
  bool IsArrayType() const           { return m_Type->IsArrayType(); }
  bool IsClassType() const           { return m_Type->IsClassType(); }
  bool IsFunctionType() const        { return m_Type->IsFunctionType(); }
  bool IsFundamentalType() const     { return m_Type->IsFundamentalType(); }
  bool IsPointerType() const         { return m_Type->IsPointerType(); }
  bool IsPointerToMemberType() const { return m_Type->IsPointerToMemberType(); }
  bool IsReferenceType() const       { return m_Type->IsReferenceType(); }
  //@}

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
