#include "cxxTypes.h"

namespace _cxx_
{


/**
 * Don't call this.  CvQualifiedType instances should be obtained from
 * Type::GetCvQualifiedType().
 * Constructor takes a pointer to the type to which these qualifiers
 * refer.  It defaults all qualifier flags to false.
 */
CvQualifiedType
::CvQualifiedType(const Type* in_type):
  m_Type(in_type),
  m_Const(false),
  m_Volatile(false)
{
}


/**
 * Don't call this.  CvQualifiedType instances should be obtained from
 * Type::GetCvQualifiedType().
 * This constructor takes a pointer to the type to which these qualifiers
 * refer, and settings for the const and volatile cv-qualifier flags.
 */
CvQualifiedType
::CvQualifiedType(const Type* in_type, bool in_const, bool in_volatile):
  m_Type(in_type),
  m_Const(in_const),
  m_Volatile(in_volatile)
{
}


/**
 * Copy constructor.
 */
CvQualifiedType
::CvQualifiedType(const Self& r):
  m_Type(r.m_Type),
  m_Const(r.m_Const),
  m_Volatile(r.m_Volatile)
{
}


/**
 * Test if this cv-qualified type can be converted to the given
 * cv-qualified type.
 */
bool
CvQualifiedType
::CanConvertTo(const Self& in_type) const
{
  return m_Type->CanConvertTo(in_type, m_Const, m_Volatile);
}


/**
 * CvQualifiedTypes compare equal iff they refer to the same Type, and
 * have the same cv-qualifiers.
 */
bool
CvQualifiedType
::operator== (const Self& r) const
{
  return ((m_Type == r.m_Type)
          && (m_Const == r.m_Const)
          && (m_Volatile == r.m_Volatile));
}


/**
 * Uniquely orders CvQualifiedType instances.  Useful for using them as
 * map keys.
 */
bool
CvQualifiedType
::operator< (const Self& r) const
{
  // First, compare the Type pointers.  Here we take advantage of the
  // fact that types are generated uniquely from the TypeSystem's factory,
  // so that the same type always has the same pointer value.
  if(m_Type < r.m_Type)
    {
    return true;
    }
  else if(m_Type > r.m_Type)
    {
    return false;
    }
  else // if(m_Type == r.m_Type)
    {
    // The base type is the same.  Compare the cv-qualifiers.
    unsigned char lhs = ((m_Const << 1) | m_Volatile);
    unsigned char rhs = ((r.m_Const << 1) | r.m_Volatile);
    return (lhs < rhs);
    }
}


} // namespace _cxx_
