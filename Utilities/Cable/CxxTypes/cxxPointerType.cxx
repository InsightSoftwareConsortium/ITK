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
 * Constructor takes the cv-qualified type to which the pointer points.
 */
PointerType
::PointerType(const CvQualifiedType& in_type):
  m_ReferencedType(in_type)
{
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


} // namespace _cxx_
