#ifndef _xmlParseException_h
#define _xmlParseException_h

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
  ParseException(const char* file, int line): m_File(file), m_Line(line) {}

  virtual void Print(std::ostream& os) const
    {
      os << "Unknown exceptions during parse." << std::endl;
    }
  void PrintLocation(std::ostream& os) const
    {
      os << "An exception occured in source file \"" << m_File.c_str()
         << "\", line " << m_Line << std::endl;
    }
private:
  String m_File;
  int m_Line;
};


/**
 * An unknown element tag was encountered.
 */
class UnknownElementTagException: public ParseException
{
public:
  UnknownElementTagException(const char* file, int line,
                             const char* unknown):
    ParseException(file, line), m_Unknown(unknown) {}

  void Print(std::ostream& os) const
    {
      os << "Unknown element tag: " << m_Unknown.c_str() << std::endl;
    }
private:
  String m_Unknown;
};

  
} // namespace xml

#endif
