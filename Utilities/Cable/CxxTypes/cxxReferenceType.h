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
  virtual RepresentationType GetRepresentationType() const;
  
  ReferenceType(const CvQualifiedType&);
  
protected:
  virtual bool CanConvertTo(const CvQualifiedType&, bool, bool, bool) const;
  
private:
  /**
   * The type to which this type refers.
   */
  CvQualifiedType m_ReferencedType;
};


} // namespace _cxx_


#endif
