#include "cxxTypes.h"

namespace _cxx_
{


/**
 * Retrieve what kind of Type this is.
 */
RepresentationType
ArrayType
::GetRepresentationType() const
{
  return ArrayType_id;
}


/**
 * Constructor takes the type of the elements in the array, and the length.
 */
ArrayType
::ArrayType(const CvQualifiedType& in_elementType, unsigned long in_length):
  m_ElementType(in_elementType),
  m_Length(in_length)
{
}


/**
 *
 */
bool
ArrayType
::CanConvertTo(const CvQualifiedType&, bool, bool, bool) const
{
  return false;
}



} // namespace _cxx_
