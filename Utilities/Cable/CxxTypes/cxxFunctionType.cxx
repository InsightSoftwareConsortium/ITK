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


/**
 * Constructor takes the return type of the function.
 */
FunctionType
::FunctionType(const CvQualifiedType& in_type):
  m_ReturnType(in_type)
{
}


} // namespace _cxx_
