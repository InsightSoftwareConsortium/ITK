#ifndef _cxxPointerToMemberType_h
#define _cxxPointerToMemberType_h

#include "cxxPointerType.h"

namespace _cxx_
{


/**
 * Represents a C++ pointer-to-member type.
 */
class PointerToMemberType: public PointerType
{
public:
  virtual RepresentationType GetRepresentationType() const;

  PointerToMemberType(const CvQualifiedType&, const ClassType*);
  
protected:
  virtual bool CanConvertTo(const CvQualifiedType&, bool, bool, bool) const;
  
private:
  /**
   * The class type holding the member.
   */
  const ClassType* m_ClassType;
};

} // namespace _cxx_


#endif
