#include "cxxTypes.h"

namespace _cxx_
{

/**
 * Retrieve what kind of Type this is.
 */
RepresentationType
PointerToMemberType
::GetRepresentationType() const
{
  return PointerToMemberType_id;
}


/**
 *
 */
bool
PointerToMemberType
::CanConvertTo(const CvQualifiedType&, bool, bool, bool) const
{
  return false;
}


/**
 * Constructor takes cv-qualified type of member, and the type of
 * the class in which the member resides.
 */
PointerToMemberType
::PointerToMemberType(const CvQualifiedType& in_type,
                      const ClassType* in_class):
  PointerType(in_type),
  m_ClassType(in_class)
{
}
  

} // namespace _cxx_
