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

class ConfigurationParser: public Object
{
public:
  typedef ConfigurationParser Self;
  typedef SmartPointer<Self> Pointer;
  typedef SmartPointer<const Self> ConstPointer;
  
  static Pointer New();
  
  void Parse(std::istream&);
  
  /**
   * Map from element name to its beginning handler.
   */
  typedef std::map<String, void (Self::*)(const Attributes&)>  BeginHandlers;
  
  /**
   * Map from element name to its ending handler.
   */
  typedef std::map<String, void (Self::*)(void)>  EndHandlers;
  
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
  XML_Parser m_XML_Parser;
  Package::Pointer m_Package;

  /**
   * Flag for whether a CDATA section is being parsed.
   */
  bool m_CdataSectionFlag;
  
  void begin_Package(const Attributes&);
  void end_Package();
  
  static void InitializeHandlers();  
  static BeginHandlers beginHandlers;
  static EndHandlers   endHandlers;  
};

} // namespace xml
  
#endif
