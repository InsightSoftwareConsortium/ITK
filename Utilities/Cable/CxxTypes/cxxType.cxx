#include "cxxTypes.h"

namespace _cxx_
{

/**
 * Given cv-qualifiers, construct the CvQualifiedType referring to
 * this type.  The result may be more cv-qualified than that given if
 * this is a typedef type.
 */
CvQualifiedType
Type
::GetCvQualifiedType(bool isConst, bool isVolatile) const
{
  // This is not a TypedefType.  Just construct the qualified form and
  // return it.
  return CvQualifiedType(this, isConst, isVolatile);
}


} // namespace _cxx_
