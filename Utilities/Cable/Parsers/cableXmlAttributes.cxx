#include "xmlAttributes.h"

#include <cstdlib>

namespace xml
{


/**
 * Set the value of a given attribute.
 */
void
Attributes
::Set(const String& a, const String& v)
{
  m_Attrs[a] = v;
}


/**
 * Get the string representation of an attribute.
 */
const char*
Attributes
::Get(const String& a) const
{
  if(m_Attrs.count(a) > 0)
    {
    return m_Attrs.find(a)->second.c_str();
    }
  else
    {
    throw MissingAttributeException(a.c_str());
    }
}


/**
 * Get an attribute with conversion to integer.
 */
int
Attributes
::GetAsInteger(const String& a) const
{
  return std::atoi(this->Get(a));
}


/**
 * Get an attribute with conversion to boolean.
 */
bool
Attributes
::GetAsBoolean(const String& a) const
{
  return (this->GetAsInteger(a) != 0);
}


/**
 * Check if an attribute is available.
 */
bool
Attributes
::Have(const String& a) const
{
  return (m_Attrs.count(a) > 0);
}


} // namespace xml
