#include "cxxTypes.h"

namespace _cxx_
{


/**
 * Retrieve what kind of Type this is.
 */
RepresentationType
ReferenceType
::GetRepresentationType() const
{
  return ReferenceType_id;
}


/**
 * Constructor takes the cv-qualified type that is referenced.
 */
ReferenceType
::ReferenceType(const CvQualifiedType& in_type):
  m_ReferencedType(in_type)
{
}


/**
 * Test whether this type with the given cv-qualifiers can convert to
 * the given CvQualifiedType.
 */
bool
ReferenceType
::CanConvertTo(const CvQualifiedType&, bool, bool) const
{
  return false;
}


} // namespace _cxx_
