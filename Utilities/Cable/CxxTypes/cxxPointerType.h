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
  virtual RepresentationType GetRepresentationType() const;
  
protected:
  virtual bool CanConvertTo(const CvQualifiedType&, bool, bool, bool) const;
  
  PointerType(const CvQualifiedType&);
  PointerType(const Self&) {}
  void operator=(const Self&) {}
  virtual ~PointerType() {}
  
private:
  /**
   * The type to which this type refers.
   */
  CvQualifiedType m_ReferencedType;
};

} // namespace _cxx_


#endif
