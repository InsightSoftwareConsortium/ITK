#include "cxxTypes.h"

namespace _cxx_
{


/**
 * Constructor takes a pointer to the type to which these qualifiers
 * refer.  It defaults all qualifier flags to false.
 */
CvQualifiedType
::CvQualifiedType(const Type* in_type):
  m_Type(in_type),
  m_Const(false),
  m_Volatile(false),
  m_Restrict(false)
{
}


/**
 * Copy constructor.
 */
CvQualifiedType
::CvQualifiedType(const Self& r):
  m_Type(r.m_Type),
  m_Const(r.m_Const),
  m_Volatile(r.m_Volatile),
  m_Restrict(r.m_Restrict)
{
}

} // namespace _cxx_
