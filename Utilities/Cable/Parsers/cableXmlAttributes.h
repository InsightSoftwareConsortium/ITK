#ifndef _xmlAttributes_h
#define _xmlAttributes_h

#include "xmlParseException.h"

#include <map>

namespace xml
{


/**
 * Store the set of attributes provided to an element tag, and their values.
 */
class Attributes
{
public:
  void Set(const String& a, const String& v);
  const char* Get(const String& a) const;
  int GetAsInteger(const String& a) const;
  bool GetAsBoolean(const String& a) const;
  bool Have(const String& a) const;
  
private:
  /**
   * Map from attribute name to its value.
   */
  std::map<String, String>  m_Attrs;
};


/**
 * An attribute requested from an element begin tag is not known.
 */
class UnknownAttributeException: public ParseException
{
public:
  UnknownAttributeException(const char* file, int line,
                            const char* unknown):
    ParseException(file, line), m_Unknown(unknown) {}
  
  void Print(std::ostream& os) const
    {
      os << "Unknown element attribute: " << m_Unknown.c_str();
    }
private:
  String m_Unknown;
};


} // namespace xml
  
#endif
