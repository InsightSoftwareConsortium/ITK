/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxType.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _cxxType_h
#define _cxxType_h

#include "cxxTypes.h"

namespace _cxx_
{

class CvQualifiedType;

/**
 * Abstract interface to a C++ type representation.
 */
class _cxx_EXPORT Type
{
public:
  /**
   * Retrieve what kind of Type this is.
   */
  virtual RepresentationType GetRepresentationType() const = 0;
  virtual bool IsTypedefType() const;
  
  /*@{
   * Quick type representation test.
   */     
  bool IsArrayType() const           { return this->GetRepresentationType() == ArrayType_id; }
  bool IsClassType() const           { return this->GetRepresentationType() == ClassType_id; } 
  bool IsFunctionType() const        { return this->GetRepresentationType() == FunctionType_id; }
  bool IsFundamentalType() const     { return this->GetRepresentationType() == FundamentalType_id; }
  bool IsPointerType() const         { return this->GetRepresentationType() == PointerType_id; }
  bool IsPointerToMemberType() const { return this->GetRepresentationType() == PointerToMemberType_id; }
  bool IsReferenceType() const       { return this->GetRepresentationType() == ReferenceType_id; }
  bool IsEitherPointerType() const   { return (this->IsPointerType() || this->IsPointerToMemberType()); }
  //@}
  
  virtual const Type* Id() const;  
  virtual CvQualifiedType GetCvQualifiedType(bool, bool) const;

  /**
   * Get the name of the type.
   */
  String Name() const;
  String CvName(bool isConst, bool isVolatile) const;

  virtual String GenerateName(const String& indirection,
                              bool isConst, bool isVolatile) const =0;
  
protected:
  Type() {}
  virtual ~Type() {}
  String GetLeftCvString(bool isConst, bool isVolatile) const;
  String GetRightCvString(bool isConst, bool isVolatile) const;
};


/**
 * An exception of this type is thrown when a representation's
 * SafeDownCast fails.
 */
class TypeDownCastException
{
public:
  TypeDownCastException(RepresentationType from, RepresentationType to);
  TypeDownCastException(const Type* from, RepresentationType to);
  
  String GetMessage() const;
private:
  RepresentationType m_From;
  RepresentationType m_To;
};

  
} // namespace _cxx_

#endif
