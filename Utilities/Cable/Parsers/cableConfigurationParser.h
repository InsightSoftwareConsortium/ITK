#ifndef _configurationParser_h
#define _configurationParser_h

#include "xmlParseException.h"
#include "xmlAttributes.h"
#include "configRep.h"

#include <stack>

namespace configuration
{

typedef ::xml::ParseException              ParseException;
typedef ::xml::UnknownElementTagException  UnknownElementTagException;
typedef ::xml::Attributes                  Attributes;

/**
 * Class to parse the XML wrapper configuration file.
 */
class Parser: public Object
{
public:
  typedef Parser Self;
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
  Parser();
  Parser(const Self&) {}
  void operator=(const Self&) {}
  virtual ~Parser();
  
private:
  /**
   * The actual XML_Parser.
   */
  XML_Parser m_XML_Parser;
  
  /**
   * A stack of XML elements used during parsing of the XML configuration.
   */
  std::stack<ConfigureObject::Pointer>  m_ElementStack;  
  
  /**
   * Store the package configuration instance.
   */
  Package::Pointer m_Package;
  
  /**
   * Flag for whether a CDATA section is being parsed.
   */
  bool m_CdataSectionFlag;
  
  /**
   * Set of named CreateMethod s that have been defined.
   */
  std::map<String, CodeBlock::Pointer>  m_CreateMethods;
  
  /**
   * Set of named DeleteMethod s that have been defined.
   */
  std::map<String, CodeBlock::Pointer>  m_DeleteMethods;

  /**
   * Set of named ArgumentSet s that have been defined.
   */
  std::map<String, ArgumentSet::Pointer>  m_ArgumentSets;

  // Access functions for element stack.
  ConfigureObject::Pointer CurrentElement();
  Package::Pointer         CurrentPackage();
  Dependencies::Pointer    CurrentDependencies();
  CodeBlock::Pointer       CurrentCodeBlock();
  ArgumentSet::Pointer     CurrentArgumentSet();
  Argument::Pointer        CurrentArgument();
  Headers::Pointer         CurrentHeaders();

  // Element stack utilities.
  void PushElement(ConfigureObject* element);
  void PopElement();

  // The element begin handlers.
  void begin_Package(const Attributes&);
  void begin_Dependencies(const Attributes&);
  void begin_CreateMethod(const Attributes&);
  void begin_DeleteMethod(const Attributes&);
  void begin_ArgumentSet(const Attributes&);
  void begin_Argument(const Attributes&);
  void begin_Headers(const Attributes&);
  void begin_File(const Attributes&);
  void begin_Directory(const Attributes&);
  
  // The element end handlers.
  void end_Package();
  void end_Dependencies();
  void end_CreateMethod();
  void end_DeleteMethod();
  void end_ArgumentSet();
  void end_Argument();
  void end_Headers();
  void end_File();
  void end_Directory();
  
  // Element map utilities.
  static void InitializeHandlers();
  
  /**
   * Map from element name to its beginning handler.
   */
  typedef std::map<String, void (Self::*)(const Attributes&)>  BeginHandlers;
  
  /**
   * Map from element name to its ending handler.
   */
  typedef std::map<String, void (Self::*)()>  EndHandlers;

  static BeginHandlers beginHandlers;
  static EndHandlers   endHandlers;  

  // Proxy functions for call-backs from XML Parser.
  static void BeginElement_proxy(void*, const char *, const char **);
  static void EndElement_proxy(void*, const char *);
  static void CharacterDataHandler_proxy(void*,const XML_Char *, int);
  static void BeginCdataSectionHandler_proxy(void*);
  static void EndCdataSectionHandler_proxy(void*);
};

} // namespace configuration
  
#endif
