/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cableConfigurationParser.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _configurationParser_h
#define _configurationParser_h

#include "configRep.h"
#include "xmlParseException.h"
#include "xmlAttributes.h"

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
  
  const CableConfiguration* GetCableConfiguration() const
    { return m_CableConfiguration.RealPointer(); }
  
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
   * A stack of Namespace elements to keep track of the current Namespace
   * scope in the configuration file.
   */
  std::stack<Namespace::Pointer>  m_NamespaceStack;
  
  /**
   * Store the CableConfiguration instance.
   */
  CableConfiguration::Pointer m_CableConfiguration;
  
  /**
   * Flag for whether a CDATA section is being parsed.
   */
  bool m_CdataSectionFlag;  

  // Access functions for element stack.
  ConfigureObject::Pointer  TopParseElement() const;
  Package::Pointer          CurrentPackage() const;
  Dependencies::Pointer     CurrentDependencies() const;
  CodeBlock::Pointer        CurrentCodeBlock() const;
  Set::Pointer              CurrentSet() const;
  Element::Pointer          CurrentElement() const;
  Headers::Pointer          CurrentHeaders() const;

  // Element stack utilities.
  void PushElement(ConfigureObject*);
  void PopElement();
  
  // Namespace stack utilities.
  void PushNamespace(Namespace*);
  void PopNamespace();
  Namespace::Pointer        CurrentNamespaceScope() const;
  PackageNamespace::Pointer CurrentPackageNamespaceScope() const;

  // The element begin handlers.
  void begin_CableConfiguration(const Attributes&);
  void begin_Package(const Attributes&);
  void begin_Dependencies(const Attributes&);
  void begin_Headers(const Attributes&);
  void begin_File(const Attributes&);
  void begin_Directory(const Attributes&);
  void begin_Namespace(const Attributes&);
  void begin_Code(const Attributes&);
  void begin_Set(const Attributes&);
  void begin_Element(const Attributes&);
  void begin_WrapperSet(const Attributes&);
  void begin_InstantiationSet(const Attributes&);
  
  // The element end handlers.
  void end_CableConfiguration();
  void end_Package();
  void end_Dependencies();
  void end_Headers();
  void end_File();
  void end_Directory();
  void end_Namespace();
  void end_Code();
  void end_Set();
  void end_Element();
  void end_WrapperSet();
  void end_InstantiationSet();
  
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
  
  // Other utilities.
  Namespace* GlobalNamespace() const;
  void GenerateElementCombinations(const Element*, Set*) const;
};

} // namespace configuration
  
#endif
