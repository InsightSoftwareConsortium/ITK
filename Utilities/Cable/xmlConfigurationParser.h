#ifndef _xmlConfigurationParser_h
#define _xmlConfigurationParser_h

#include <iostream>
#include <cstdio>
#include <map>
#include <stack>
#include <cstdlib>

#include "parseUtils.h"
#include "configRep.h"

namespace xml
{

/**
 * Class to parse the XML wrapper configuration file.
 */
class ConfigurationParser: public Object
{
public:
  typedef ConfigurationParser Self;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  
  static Pointer New();
  
  void Parse(std::istream&);
  
  // Call-backs from the XML parser.
  void BeginElement(const char *name, const char **atts);
  void EndElement(const char *name);
  void BeginCdataSectionHandler();
  void EndCdataSectionHandler();
  void CharacterDataHandler(const XML_Char *data, int length);
  
protected:
  ConfigurationParser();
  ConfigurationParser(const Self&) {}
  void operator=(const Self&) {}
  virtual ~ConfigurationParser();
  
private:
  /**
   * The actual XML_Parser.
   */
  XML_Parser m_XML_Parser;
  
  /**
   * Store the package configuration instance.
   */
  Package::Pointer m_Package;

  /**
   * Flag for whether a CDATA section is being parsed.
   */
  bool m_CdataSectionFlag;
  
  // The element begin handlers.
  void begin_Package(const Attributes&);
  
  // The element end handlers.
  void end_Package();
  
  // Element map utilities.
  static void InitializeHandlers();  
  
  /**
   * Map from element name to its beginning handler.
   */
  typedef std::map<String, void (Self::*)(const Attributes&)>  BeginHandlers;
  
  /**
   * Map from element name to its ending handler.
   */
  typedef std::map<String, void (Self::*)(void)>  EndHandlers;

  static BeginHandlers beginHandlers;
  static EndHandlers   endHandlers;  

  // Proxy functions for call-backs from XML Parser.
  static void BeginCdataSectionHandler_proxy(void*);
  static void EndCdataSectionHandler_proxy(void*);
  static void BeginElement_proxy(void*, const char *, const char **);
  static void EndElement_proxy(void*, const char *);
  static void CharacterDataHandler_proxy(void*,const XML_Char *, int);
};

} // namespace xml
  
#endif
