#include "cxxTypes.h"

namespace _cxx_
{

/**
 * Retrieve what kind of Type this is.
 */
RepresentationType
FundamentalType
::GetRepresentationType() const
{
  return FundamentalType_id;
}


/**
 * Constructor takes the Id of the fundamental type.
 */
FundamentalType::FundamentalType(Id in_id):
  m_Id(in_id)
{
}


/**
 *
 */
bool
FundamentalType
::CanConvertTo(const CvQualifiedType&, bool, bool, bool) const
{
  return false;
}


} // namespace _cxx_
