/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cableConfigurationParser.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#ifndef _cableConfigurationParser_h
#define _cableConfigurationParser_h

#include "cableConfigurationRepresentation.h"
#include "cableXmlParseException.h"
#include "cableXmlAttributes.h"

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
  
  CableConfiguration::ConstPointer GetCableConfiguration() const
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
  ConfigureObject::Pointer    TopParseElement() const;
  CableConfiguration::Pointer CurrentCableConfiguration() const;
  CodeBlock::Pointer          CurrentCodeBlock() const;
  Class::Pointer              CurrentClass() const;

  // Element stack utilities.
  void PushElement(ConfigureObject*);
  void PopElement();
  
  // Namespace stack utilities.
  void PushNamespace(Namespace*);
  void PopNamespace();
  Namespace::Pointer        CurrentNamespaceScope() const;

  // The element begin handlers.
  void begin_CableConfiguration(const Attributes&);
  void begin_Namespace(const Attributes&);
  void begin_Code(const Attributes&);
  void begin_Class(const Attributes&);
  void begin_Header(const Attributes&);
  void begin_Group(const Attributes&);
  
  // The element end handlers.
  void end_CableConfiguration();
  void end_Namespace();
  void end_Code();
  void end_Class();
  void end_Header();
  void end_Group();
  
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
};

} // namespace configuration
  
#endif
