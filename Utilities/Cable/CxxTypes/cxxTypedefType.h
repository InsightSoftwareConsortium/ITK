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
  virtual CvQualifiedType GetCvQualifiedType(bool, bool) const;
  
protected:
  virtual bool CanConvertTo(const CvQualifiedType&, bool, bool) const;

  TypedefType(const CvQualifiedType&);
  TypedefType(const Self&): m_CvQualifiedType(NULL) {}
  void operator=(const Self&) {}
  virtual ~TypedefType() {}
  
private:
  /**
   * The type to which the typedef refers, including optional cv-qualifiers.
   */
  CvQualifiedType m_CvQualifiedType;
  
  /**
   * Let the CvQualifiedType class call CanConvertTo.
   */
  friend CvQualifiedType;
};


} // namespace _cxx_

#endif
