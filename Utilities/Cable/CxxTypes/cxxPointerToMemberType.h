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
  typedef PointerToMemberType Self;
  
  /**
   * Retrieve what kind of Type this is.
   */
  virtual RepresentationType GetRepresentationType() const
    { return PointerToMemberType_id; }

  PointerToMemberType(const CvQualifiedType& in_type,
                      const ClassType* in_class):
    PointerType(in_type), m_ClassType(in_class) {}
  
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
