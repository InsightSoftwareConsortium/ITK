#ifndef _cxxType_h
#define _cxxType_h

#include "cxxTypes.h"

namespace _cxx_
{

class CvQualifiedType;

/**
 * Abstract interface to a C++ type representation.
 */
class Type
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
  //@}

  virtual CvQualifiedType GetCvQualifiedType(bool, bool) const;
  
  virtual bool CanConvertTo(const CvQualifiedType&, bool, bool) const = 0;
  
protected:

  Type() {}
  virtual ~Type() {}
  
  /**
   * Let the CvQualifiedType class call CanConvertTo.
   */
  friend CvQualifiedType;
};


} // namespace _cxx_

#endif
