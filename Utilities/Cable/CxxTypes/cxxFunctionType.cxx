#include "cxxTypes.h"

namespace _cxx_
{

/**
 * Retrieve what kind of Type this is.
 */
RepresentationType
FunctionType
::GetRepresentationType() const
{
  return FunctionType_id;
}

/**
 *
 */
bool
FunctionType
::CanConvertTo(const CvQualifiedType&, bool, bool, bool) const
{
  return false;
}


} // namespace _cxx_
