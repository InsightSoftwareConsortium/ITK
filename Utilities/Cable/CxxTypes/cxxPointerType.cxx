#include "cxxTypes.h"

namespace _cxx_
{


/**
 * Retrieve what kind of Type this is.
 */
RepresentationType
PointerType
::GetRepresentationType() const
{
  return PointerType_id;
}


/**
 *
 */
bool
PointerType
::CanConvertTo(const CvQualifiedType&, bool, bool, bool) const
{
  return false;
}


/**
 * Constructor takes the cv-qualified type to which the pointer points.
 */
PointerType
::PointerType(const CvQualifiedType& in_type):
  m_ReferencedType(in_type)
{
}


} // namespace _cxx_
