#ifndef _parseUtils_h
#define _parseUtils_h

#include <cstdio>
#include <cstdlib>
#include <assert.h>

#include <map>
#include <string>

#include "xmlparse.h"

typedef std::string String;

/**
 * Define the interface to exceptions thrown during parsing.
 */
class ParseException
{
public:
  ParseException(const char* file, int line): m_File(file), m_Line(line) {}

  virtual void Print(FILE* f) const
    {
      fprintf(f, "Unknown parse error.\n");
    }
  void PrintLocation(FILE* f) const
    {
      fprintf(f, "An error occured in source file \"%s\", line %d:\n",
              m_File.c_str(), m_Line);
    }
private:
  String m_File;
  int m_Line;
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
  
  void Print(FILE* f) const
    {
      fprintf(f, "Unknown element attribute: %s\n",
              m_Unknown.c_str());
    }
private:
  String m_Unknown;
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

  void Print(FILE* f) const
    {
      fprintf(f, "Unknown element tag: %s\n",
              m_Unknown.c_str());
    }
private:
  String m_Unknown;
};


/**
 * Store the set of attributes provided to an element tag, and their values.
 */
class Attributes
{
public:
  /**
   * Set the value of a given attribute.
   */
  void Set(String a, const String& v)
    {
      m_Attrs[a] = v;
    }

  /**
   * Get the string representation of an attribute.
   */
  const char* Get(String a) const
    {
      if(m_Attrs.count(a) > 0)
        {
        return m_Attrs.find(a)->second.c_str();
        }
      else
        {
        throw UnknownAttributeException(__FILE__, __LINE__, a.c_str());
        }
    }

  
  /**
   * Get an attribute with conversion to integer.
   */
  int GetAsInteger(String a) const
    {
      return atoi(this->Get(a));
    }
  
  /**
   * Get an attribute with conversion to boolean.
   */
  bool GetAsBoolean(String a) const
    {
      return (this->GetAsInteger(a) != 0);
    }
  
private:
  std::map<String, String>  m_Attrs;
};


/**
 * Map from element name to its beginning handler.
 */
typedef std::map<String, void (*)(const Attributes&)>  BeginHandlers;

/**
 * Map from element name to its ending handler.
 */
typedef std::map<String, void (*)(void)>  EndHandlers;


/**
 * Parser's user data.  Stores pointers to begin and end handlers.
 */
class HandlersPair
{
public:
  HandlersPair(BeginHandlers* b, EndHandlers* e):
    beginHandlers(b), endHandlers(e) {}

  BeginHandlers*  beginHandlers;
  EndHandlers*    endHandlers;
};

extern void StartElement(void *, const char *, const char **);
extern void EndElement(void *, const char *);


#endif
