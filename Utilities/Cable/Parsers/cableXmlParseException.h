#ifndef _xmlParseException_h
#define _xmlParseException_h

#include "win32Header.h"

#include "xmlparse.h"

#include <iostream>
#include <string>

namespace xml
{

typedef std::string String;

/**
 * Define the interface to exceptions thrown during parsing.
 */
class ParseException
{
public:
  ParseException() {}
  virtual ~ParseException() {}
  
  virtual void Print(std::ostream& os) const
    {
      os << "Unknown exceptions during parse." << std::endl;
    }

  void PrintLocation(std::ostream& os, XML_Parser parser,
                     const char* source = 0) const
    {
      if(source) os << source << ": ";
      os << "Error at line " << XML_GetCurrentLineNumber(parser)
         << ", column " << XML_GetCurrentColumnNumber(parser)
         << std::endl;
    }  
};


/**
 * An unknown element tag was encountered.
 */
class UnknownElementTagException: public ParseException
{
public:
  UnknownElementTagException(const char* unknown):
    ParseException(), m_Unknown(unknown) {}
  virtual ~UnknownElementTagException() {}

  void Print(std::ostream& os) const
    {
      os << "Unknown element tag: " << m_Unknown.c_str() << std::endl;
    }
private:
  String m_Unknown;
};

  
} // namespace xml

#endif
