#include "cxxTypes.h"

namespace _cxx_
{


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
