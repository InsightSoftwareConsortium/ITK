/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cableConfigurationParser.cxx
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
#include "cableConfigurationParser.h"

namespace configuration
{

/**
 * Create a new Parser and return a pointer to it.
 */
Parser::Pointer
Parser::New()
{
  return new Self;
}


/**
 * Constructor sets up an XML_Parser to make call-backs into this
 * Parser.
 */
Parser
::Parser():
  m_XML_Parser(XML_ParserCreate(NULL)),
  m_CableConfiguration(NULL),
  m_CdataSectionFlag(false)
{
  Parser::InitializeHandlers();
  
  XML_SetElementHandler(m_XML_Parser,
                        BeginElement_proxy,
                        EndElement_proxy);
  XML_SetUserData(m_XML_Parser,
                  this);
  XML_SetCdataSectionHandler(m_XML_Parser,
                             BeginCdataSectionHandler_proxy,
                             EndCdataSectionHandler_proxy);
  XML_SetCharacterDataHandler(m_XML_Parser,
                              CharacterDataHandler_proxy);
}


/**
 * Cleanup the XML_Parser
 */
Parser
::~Parser()
{
  XML_ParserFree(m_XML_Parser);
}


/**
 * Parse the XML from the given input stream until end-of-input is reached.
 */
void
Parser
::Parse(std::istream& inStream)
{
  char buf[257];
  bool done = false;

  /**
   * Parse until end of input stream is reached.
   */
  while(!done)
    {
    inStream.read(buf, sizeof(buf));
    size_t len = inStream.gcount();
    done = (len < sizeof(buf));
    if(!XML_Parse(m_XML_Parser, buf, len, done))
      {
      std::cerr << "Parser::Parse(): \""
                << XML_ErrorString(XML_GetErrorCode(m_XML_Parser))
                << "\" at source line "
                << XML_GetCurrentLineNumber(m_XML_Parser)
                << std::endl;
      }
    }
}


/**
 * Called when a new element is opened in the XML source.
 * Checks the tag name, and calls the appropriate handler with an
 * Attributes container.
 */
void
Parser
::BeginElement(const char *name, const char **atts)
{
  try {
  // Try to look up the handler for this element.
  BeginHandlers::iterator handler = beginHandlers.find(name);
  if(handler != beginHandlers.end())
    {
    // Found one, call it.
    Attributes attributes;
    for(int i=0; atts[i] && atts[i+1] ; i += 2)
      {
      attributes.Set(atts[i], atts[i+1]);
      }
    (this->*(handler->second))(attributes);
    }
  else
    {
    throw UnknownElementTagException(name);
    }
  }
  catch (const ParseException& e)
    {
    e.PrintLocation(std::cerr, m_XML_Parser, "configuration");
    e.Print(std::cerr);
    }
  catch (const String& e)
    {
    std::cerr << "Caught exception in Parser::BeginElement():"
              << std::endl << e.c_str() << std::endl;
    }
  catch (...)
    {
    std::cerr << "Caught unknown exception in Parser::BeginElement()."
              << std::endl;
    }
}


/**
 * Called at the end of an element in the XML source opened when
 * BeginElement was called.
 */
void
Parser
::EndElement(const char *name)
{
  try {
  // Try to look up the handler for this element.
  EndHandlers::iterator handler = endHandlers.find(name);
  if(handler != endHandlers.end())
    {
    // Found one, call it.
    (this->*(handler->second))();
    }
  else
    {
    throw UnknownElementTagException(name);
    }
  }
  catch (const ParseException& e)
    {
    e.PrintLocation(std::cerr, m_XML_Parser, "configuration");
    e.Print(std::cerr);
    }
  catch (const String& e)
    {
    std::cerr << "Caught exception in Parser::EndElement():"
              << std::endl << e.c_str() << std::endl;
    }
  catch (...)
    {
    std::cerr << "Caught unknown exception in Parser::EndElement()."
              << std::endl;
    }
}


/**
 * Set the flag to indicate whether we are currently parsing a CDATA
 * section from the XML source.
 */
void
Parser
::BeginCdataSectionHandler()
{
  m_CdataSectionFlag = true;
}


/**
 * Get the flag to indicate whether we are currently parsing a CDATA
 * section from the XML source.
 */
void
Parser
::EndCdataSectionHandler()
{
  m_CdataSectionFlag = false;
}


/**
 * When an XML character data segment is encountered, this is called
 * to handle it.
 *
 * Such a segment is of the form:
 * <![CDATA[....]]>
 * or just
 * ....
 * "data" will be ...., one line at a time.
 * "data" will NOT be '\0' terminated, but "length" specifies how much.
 */
void
Parser
::CharacterDataHandler(const XML_Char *data, int length)
{
  this->TopParseElement()->AddCharacterData(data, length, m_CdataSectionFlag);
}


/**
 * Get the top of the element stack.
 */
ConfigureObject::Pointer
Parser
::TopParseElement() const
{
  return m_ElementStack.top();
}


/**
 * An invalid identifier has been used.
 */
class InvalidIdentifierException: public ParseException
{
public:
  InvalidIdentifierException(const String& invalid):
    ParseException(), m_Invalid(invalid) {}
  virtual ~InvalidIdentifierException() {}
  void Print(std::ostream& os) const
    {
      os << "Invalid identifier \"" << m_Invalid.c_str() << "\""
         << std::endl;
    }
private:
  String m_Invalid;
};


/**
 * An attempt to duplicate an existing name has occurred.
 */
class NameExistsException: public ParseException
{
public:
  NameExistsException(const String& name):
    ParseException(), m_Name(name) {}
  virtual ~NameExistsException() {}
  void Print(std::ostream& os) const
    {
      os << "Name \"" << m_Name.c_str()
         << "\" already exists locally in this scope."
         << std::endl;
    }
private:
  String m_Name;
};


/**
 * An unexpected element type is on top of the stack.
 */
class ElementStackTypeException: public ParseException
{
public:
  ElementStackTypeException(const char* e, const char* t):
    ParseException(), m_Expected(e), m_Got(t) {}
  virtual ~ElementStackTypeException() {}
  void Print(std::ostream& os) const
    {
      os << "Expected \"" << m_Expected.c_str()
         << "\" as immediate enclosing element, but got \"" << m_Got << "\""
         << std::endl;
    }
private:
  String m_Expected;
  String m_Got;
};


/**
 * An unexpected namespace type is on top of the namespace stack.
 */
class NamespaceStackTypeException: public ParseException
{
public:
  NamespaceStackTypeException(const char* e, const char* t):
    ParseException(), m_Expected(e), m_Got(t) {}
  virtual ~NamespaceStackTypeException() {}
  void Print(std::ostream& os) const
    {
      os << "Expected \"" << m_Expected.c_str()
         << "\" as current namespace scope, but got \"" << m_Got << "\""
         << std::endl;
    }
private:
  String m_Expected;
  String m_Got;
};


/**
 * Get the current CableConfiguration off the top of the element stack.
 */
CableConfiguration::Pointer
Parser
::CurrentCableConfiguration() const
{
  if(!m_ElementStack.top()->IsCableConfiguration())
    throw ElementStackTypeException("CableConfiguration",
                                    m_ElementStack.top()->GetClassName());

  return dynamic_cast<CableConfiguration*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the current CodeBlock off the top of the element stack.
 */
CodeBlock::Pointer
Parser
::CurrentCodeBlock() const
{
  if(!m_ElementStack.top()->IsCodeBlock())
    throw ElementStackTypeException("CodeBlock",
                                    m_ElementStack.top()->GetClassName());

  return dynamic_cast<CodeBlock*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the current Class off the top of the element stack.
 */
Class::Pointer
Parser
::CurrentClass() const
{
  if(!m_ElementStack.top()->IsClass())
    throw ElementStackTypeException("Class",
                                    m_ElementStack.top()->GetClassName());

  return dynamic_cast<Class*>(m_ElementStack.top().RealPointer());
}


/**
 * Push a new element onto the element stack.
 */
void
Parser
::PushElement(ConfigureObject* element)
{
  m_ElementStack.push(element);
}


/**
 * Pop the top off the element stack.
 */
void
Parser
::PopElement()
{
  m_ElementStack.pop();

  // Sanity check.
  if(m_ElementStack.empty())
    {
    throw String("CableConfiguration popped from element stack!");
    }
}


/**
 * Push a new namespace onto the namespace stack.
 */
void
Parser
::PushNamespace(Namespace* ns)
{
  m_NamespaceStack.push(ns);
}


/**
 * Pop the top off the namespace stack.
 */
void
Parser
::PopNamespace()
{
  m_NamespaceStack.pop();

  // Sanity check.
  if(m_NamespaceStack.empty())
    {
    throw String("Global namespace popped from namespace stack!");
    }
}



/**
 * Get the current Namespace scope.  This is the top of the Namespace
 * stack.
 */
Namespace::Pointer
Parser
::CurrentNamespaceScope() const
{
  return m_NamespaceStack.top();
}


/**
 * Begin handler for CableConfiguration element.
 */
void
Parser
::begin_CableConfiguration(const Attributes& atts)
{
  String source;
  String group;
  String package;
  if(atts.Have("source"))
    {
    source = atts.Get("source");
    }
  if(atts.Have("package"))
    {
    package = atts.Get("package");
    group = package;
    }
  if(atts.Have("group"))
    {
    group = atts.Get("group");
    }
 
  // This is the outermost element.
  m_CableConfiguration = CableConfiguration::New(source, group, package);
  this->PushElement(m_CableConfiguration);
  
  // Push the global namespace onto the namespace stack.
  this->PushNamespace(this->GlobalNamespace());
}

/**
 * End handler for CableConfiguration element.
 */
void
Parser
::end_CableConfiguration()
{
  // Don't pop off the main configuration element!
}


/**
 * Return true only if the given name is a valid C++ identifier (unqualified).
 */
bool IsValidCxxIdentifier(const String& name)
{
  for(String::const_iterator c = name.begin(); c != name.end(); ++c)
    {
    char ch = *c;
    if(!(((ch >= 'a') && (ch <= 'z'))
         || ((ch >= 'A') && (ch <= 'Z'))
         || ((ch >= '0') && (ch <= '9'))
         || (ch == '_')))
      {
      return false;
      }
    }
  return true;
}


/**
 * Begin handler for Namespace element.
 */
void
Parser
::begin_Namespace(const Attributes& atts)
{
  String name = atts.Get("name");
  
  // See if the namespace exists.
  Namespace::Pointer ns = this->CurrentNamespaceScope()->LookupNamespace(name);
  
  if(!ns)
    {
    // The namespace could not be found.  We must create it.
    // This requires:
    //  - name must not be qualified.
    //  - we are currently in a Namespace element.
    
    if(IsValidCxxIdentifier(name))
      {
      Namespace* enclosingNamespace = this->CurrentNamespaceScope();      
      ns = Namespace::New(name, enclosingNamespace);
      enclosingNamespace->AddNamespace(ns);
      }
    else
      {
      throw InvalidIdentifierException(name);
      }
    }  
  
  // Open the namespace with the given name.
  this->PushNamespace(ns);
}


/**
 * End handler for Namespace element.
 */
void
Parser
::end_Namespace()
{
  // Close the namespace scope.
  this->PopNamespace();
}


/**
 * Begin handler for Code element.
 */
void
Parser
::begin_Code(const Attributes& atts)
{
  String name = atts.Get("name");
  
  // Create a new CodeBlock to hold the lines of code.
  CodeBlock::Pointer newCodeBlock = CodeBlock::New(name);
  
  // Save the CodeBlock by this name.
  if(!this->CurrentNamespaceScope()->AddCode(newCodeBlock))
    {
    throw NameExistsException(name);
    }
  
  // Put new CodeBlock on the stack so it can be filled with lines.
  this->PushElement(newCodeBlock);
}


/**
 * End handler for Code element.
 */
void
Parser
::end_Code()
{
  // Take the CodeBlock off the stack.
  this->PopElement();
}


/**
 * Begin handler for Class element.
 */
void
Parser
::begin_Class(const Attributes& atts)
{
  String name = atts.Get("name");
  
  // Create a new Class.
  Class::Pointer newClass = Class::New(name);
  
  // Put new Class on the stack so it can be filled.
  this->PushElement(newClass);
}


/**
 * End handler for Class element.
 */
void
Parser
::end_Class()
{
  Class::Pointer c = this->CurrentClass();
  
  // Take the Class off the stack.
  this->PopElement();
  
  this->CurrentNamespaceScope()->AddClass(c);
}


/**
 * Begin handler for Group element.
 */
void
Parser
::begin_Group(const Attributes& atts)
{
  String name = atts.Get("name");
  
  this->CurrentCableConfiguration()->AddGroup(name);
}


/**
 * End handler for Group element.
 */
void
Parser
::end_Group()
{
}


/**
 * Begin handler for Header element.
 */
void
Parser
::begin_Header(const Attributes& atts)
{
  String name = atts.Get("name");
  
  this->CurrentCableConfiguration()->AddHeader(name);
}


/**
 * End handler for Header element.
 */
void
Parser
::end_Header()
{
}


/**
 * Map of Parser element begin handlers.
 */
Parser::BeginHandlers Parser::beginHandlers;

/**
 * Map of Parser element end handlers.
 */
Parser::EndHandlers Parser::endHandlers;

/**
 * This static method initializes the beginHandlers and endHandlers
 * maps.  It is called by the Parser constructor every time,
 * but only initializes the maps once.
 */
void
Parser
::InitializeHandlers()
{
  // Make sure we only initialize the maps once.
  static bool initialized = false;  
  if(initialized) return;
  
  beginHandlers["CableConfiguration"] = &Parser::begin_CableConfiguration;
  beginHandlers["Namespace"]          = &Parser::begin_Namespace;
  beginHandlers["Code"]               = &Parser::begin_Code;
  beginHandlers["Class"]              = &Parser::begin_Class;
  beginHandlers["Header"]             = &Parser::begin_Header;
  beginHandlers["Group"]              = &Parser::begin_Group;

  endHandlers["CableConfiguration"] = &Parser::end_CableConfiguration;
  endHandlers["Namespace"]          = &Parser::end_Namespace;
  endHandlers["Code"]               = &Parser::end_Code;
  endHandlers["Class"]              = &Parser::end_Class;
  endHandlers["Header"]             = &Parser::end_Header;
  endHandlers["Group"]              = &Parser::end_Group;
  
  initialized = true;
}


/**
 * Passes call through to real parser object according to first argument.
 */
void
Parser
::BeginElement_proxy(void* parser, const char *name, const char **atts)
{
  static_cast<Parser*>(parser)->BeginElement(name, atts);
}


/**
 * Passes call through to real parser object according to first argument.
 */
void
Parser
::EndElement_proxy(void* parser, const char *name)
{
  static_cast<Parser*>(parser)->EndElement(name);
}


/**
 * Passes call through to real parser object according to first argument.
 */
void
Parser
::CharacterDataHandler_proxy(void* parser,const XML_Char *data, int length)
{
  static_cast<Parser*>(parser)->CharacterDataHandler(data, length);
}


/**
 * Passes call through to real parser object according to first argument.
 */
void
Parser
::BeginCdataSectionHandler_proxy(void* parser)
{
  static_cast<Parser*>(parser)->BeginCdataSectionHandler();
}


/**
 * Passes call through to real parser object according to first argument.
 */
void
Parser
::EndCdataSectionHandler_proxy(void* parser)
{
  static_cast<Parser*>(parser)->EndCdataSectionHandler();
}


/**
 * Get the global namespace in the configuration.
 */
Namespace*
Parser
::GlobalNamespace() const
{
  return m_CableConfiguration->GetGlobalNamespace().RealPointer();
}

} // namespace configuration

