#ifndef _cxxPointerType_h
#define _cxxPointerType_h

#include "cxxCvQualifiedType.h"

namespace _cxx_
{


/**
 * Represent a C++ pointer type.
 */
class PointerType: public Type
{
public:
  typedef PointerType Self;

  virtual RepresentationType GetRepresentationType() const;
  
protected:
  virtual bool CanConvertTo(const CvQualifiedType&, bool, bool) const;
  
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
