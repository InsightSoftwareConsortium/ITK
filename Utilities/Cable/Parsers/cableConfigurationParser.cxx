/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cableConfigurationParser.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "configurationParser.h"

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
 * An unknown Set has been referenced with a $ token.
 */
class UnknownSetException: public ParseException
{
public:
  UnknownSetException(const String& unknown):
    ParseException(), m_Unknown(unknown) {}
  virtual ~UnknownSetException() {}
  void Print(std::ostream& os) const
    {
      os << "Unknown Set \"" << m_Unknown.c_str() << "\""
         << std::endl;
    }
private:
  String m_Unknown;
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
 * A Set has been referenced with a $ token inside its own definition.
 */
class SetSelfReferenceException: public ParseException
{
public:
  SetSelfReferenceException(const String& set_name):
    ParseException(), m_SetName(set_name) {}
  virtual ~SetSelfReferenceException() {}
  void Print(std::ostream& os) const
    {
      os << "Set \"" << m_SetName.c_str() << "\" cannot reference itself."
         << std::endl;
    }
private:
  String m_SetName;
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
 * Get the current Package off the top of the element stack.
 */
Package::Pointer
Parser
::CurrentPackage() const
{
  if(!m_ElementStack.top()->IsPackage())
    throw ElementStackTypeException("Package",
                                    m_ElementStack.top()->GetClassName());

  return dynamic_cast<Package*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the current Dependencies off the top of the element stack.
 */
Dependencies::Pointer
Parser
::CurrentDependencies() const
{
  if(!m_ElementStack.top()->IsDependencies())
    throw ElementStackTypeException("Dependencies",
                                    m_ElementStack.top()->GetClassName());

  return dynamic_cast<Dependencies*>(m_ElementStack.top().RealPointer());
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
 * Get the current Set off the top of the element stack.
 */
Set::Pointer
Parser
::CurrentSet() const
{
  if(!m_ElementStack.top()->IsSet()
     && !m_ElementStack.top()->IsWrapperSet()
     && !m_ElementStack.top()->IsInstantiationSet())
    throw ElementStackTypeException("Set",
                                    m_ElementStack.top()->GetClassName());

  return dynamic_cast<Set*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the current Element off the top of the element stack.
 */
Element::Pointer
Parser
::CurrentElement() const
{
  if(!m_ElementStack.top()->IsElement())
    throw ElementStackTypeException("Element",
                                    m_ElementStack.top()->GetClassName());

  return dynamic_cast<Element*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the current Headers off the top of the element stack.
 */
Headers::Pointer
Parser
::CurrentHeaders() const
{
  if(!m_ElementStack.top()->IsHeaders())
    throw ElementStackTypeException("Headers",
                                    m_ElementStack.top()->GetClassName());

  return dynamic_cast<Headers*>(m_ElementStack.top().RealPointer());
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
 * Get the current PackageNamespace scope.  This is only valid inside a
 * Package element.
 */
PackageNamespace::Pointer
Parser
::CurrentPackageNamespaceScope() const
{
  Namespace* currentNamespaceScope = this->CurrentNamespaceScope();
  if(currentNamespaceScope->IsPackageNamespace())
    {
    return dynamic_cast<PackageNamespace*>(currentNamespaceScope);
    }
  else
    {
    throw NamespaceStackTypeException("PackageNamespace",
                                      currentNamespaceScope->GetClassName());
    return NULL;
    }
}


/**
 * Begin handler for CableConfiguration element.
 */
void
Parser
::begin_CableConfiguration(const Attributes&)
{
  // This is the outermost element.
  m_CableConfiguration = CableConfiguration::New();
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
 * Begin handler for Package element.
 */
void
Parser
::begin_Package(const Attributes& atts)
{
  String name = atts.Get("name");

  if(this->TopParseElement()->IsCableConfiguration())
    {
    // This is a new package definition.
    
    // Create a copy of the current namespace for the package.
    PackageNamespace::Pointer nps =
      this->CurrentNamespaceScope()->MakePackageNamespace(
        this->CurrentNamespaceScope()->GetEnclosingNamespace());
    
    // Create the package.
    Package::Pointer newPackage = Package::New(name, nps);
    
    // Add the package to the CableConfiguration.
    m_CableConfiguration->AddPackage(newPackage);
    
    // Put the package on the element stack so it can be filled in.
    this->PushElement(newPackage);
    
    // Push the Package's global namespace onto the namespace stack.
    this->PushNamespace(nps);
    }
  else if(this->TopParseElement()->IsDependencies())
    {
    // This is a package dependency element.
    // Add the dependency.
    this->CurrentDependencies()->Add(name);
    
    // Put a dummy element on the stack for the corresponding
    // end_Package to pop off.
    this->PushElement(0);
    }
  else
    {
    throw ElementStackTypeException("CableConfiguration or Dependencies",
                                    this->TopParseElement()->GetClassName());
    }
}


/**
 * End handler for Package element.
 */
void
Parser
::end_Package()
{
  this->PopElement();
  if(this->TopParseElement()->IsCableConfiguration())
    {
    // This was the end of a package definition.
    // Pop off the package's namespace from the Namespace stack.
    this->PopNamespace();
    }
}


/**
 * Begin handler for Dependencies element.
 */
void
Parser
::begin_Dependencies(const Attributes&)
{
  Package* package = this->CurrentPackage();
  
  // See if the package already has a Dependencies instance.
  Dependencies::Pointer dependencies = package->GetDependencies();
  if(!dependencies)
    {
    // Need to create a new Dependencies and give it to the package.
    dependencies = Dependencies::New();
    package->SetDependencies(dependencies);
    }
  
  // Put the Dependencies on the stack so packages can be added.
  this->PushElement(dependencies);
}


/**
 * End handler for Dependencies element.
 */
void
Parser
::end_Dependencies()
{
  this->PopElement();
}


/**
 * Begin handler for Headers element.
 */
void
Parser
::begin_Headers(const Attributes& atts)
{
  Package* package = this->CurrentPackage();

  // See if the package already has a Headers instance.
  Headers::Pointer headers = package->GetHeaders();
  if(!headers)
    {
    // Need to create a new Headers and give it to the package.
    headers = Headers::New();
    package->SetHeaders(headers);
    }
  
  // Put the Headers on the stack so files and directories can be added.
  this->PushElement(headers);
}


/**
 * End handler for Headers element.
 */
void
Parser
::end_Headers()
{
  // Take the Headers off the stack.
  this->PopElement();
}


/**
 * Begin handler for File element.
 */
void
Parser
::begin_File(const Attributes& atts)
{
  String name = atts.Get("name");
  String purpose = atts.Have("purpose") ?
    atts.Get("purpose") : "";

  // Add the file to the current set of headers.
  this->CurrentHeaders()->AddFile(name, purpose);
}


/**
 * End handler for File element.
 */
void
Parser
::end_File()
{
}


/**
 * Begin handler for Directory element.
 */
void
Parser
::begin_Directory(const Attributes& atts)
{
  String name = atts.Get("name");
  
  // Add the directory to the current set of headers.
  this->CurrentHeaders()->AddDirectory(name);
}


/**
 * End handler for Directory element.
 */
void
Parser
::end_Directory()
{
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
    //  - the prefix_seperator attribute (optional).
    //  - name must not be qualified.
    //  - we are currently in a Namespace element.
    String prefixSeperator = atts.Have("prefix_seperator")
      ? atts.Get("prefix_seperator") : "";
    
    if(IsValidCxxIdentifier(name))
      {
      Namespace* enclosingNamespace = this->CurrentNamespaceScope();
      
      if(enclosingNamespace->IsPackageNamespace())
        {
        // We are in a PackageNamespace (inside a package definition).
        // We must create a PackageNamespace.
        ns = PackageNamespace::New(
          name, prefixSeperator,
          dynamic_cast<PackageNamespace*>(enclosingNamespace));
        }
      else
        {
        // We can create a normal Namespace.
        ns = Namespace::New(name, prefixSeperator, enclosingNamespace);
        }
      
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
 * Begin handler for Set element.
 */
void
Parser
::begin_Set(const Attributes& atts)
{
  String name = atts.Get("name");
  
  // Create a new Set.
  Set::Pointer newSet = Set::New(name);
  
  // Save the Set by this name.
  if(!this->CurrentNamespaceScope()->AddSet(newSet))
    {
    throw NameExistsException(name);
    }
  
  // Put new Set on the stack so it can be filled.
  this->PushElement(newSet);
}


/**
 * End handler for Set element.
 */
void
Parser
::end_Set()
{  
  // Take the Set off the stack.
  this->PopElement();
}


/**
 * Begin handler for Element element.
 */
void
Parser
::begin_Element(const Attributes& atts)
{
  String tag = "";
  
  if(atts.Have("tag"))
    {
    tag = atts.Get("tag");
    }
    
  // Create a new Element.
  Element::Pointer newElement = Element::New(tag);
  
  // Put new Element on the stack so it can be filled with code.
  this->PushElement(newElement);
}


/**
 * End handler for Element element.
 */
void
Parser
::end_Element()
{
  // Hold onto the finished Element as it is popped off the stack.
  Element::Pointer finishedElement = this->CurrentElement();
  
  // Take the Element off the stack.
  this->PopElement();

  // Add the element to the current Set.
  this->GenerateElementCombinations(finishedElement,
                                    this->CurrentSet().RealPointer());
}


/**
 * Begin handler for WrapperSet element.
 */
void
Parser
::begin_WrapperSet(const Attributes&)
{
  // Create a new WrapperSet.
  WrapperSet::Pointer newWrapperSet = WrapperSet::New();
    
  // Save the WrapperSet.
  this->CurrentPackageNamespaceScope()->AddWrapperSet(newWrapperSet);
    
  // Put new WrapperSet on the stack so it can be filled.
  this->PushElement(newWrapperSet);
}


/**
 * End handler for WrapperSet element.
 */
void
Parser
::end_WrapperSet()
{
  // Take the WrapperSet off the stack.
  this->PopElement();
}


/**
 * Begin handler for InstantiationSet element.
 */
void
Parser
::begin_InstantiationSet(const Attributes&)
{
  // Create a new InstantiationSet.
  InstantiationSet::Pointer newInstantiationSet = InstantiationSet::New();
    
  // Save the InstantiationSet.
  this->CurrentPackageNamespaceScope()
    ->AddInstantiationSet(newInstantiationSet);
    
  // Put new InstantiationSet on the stack so it can be filled.
  this->PushElement(newInstantiationSet);
}


/**
 * End handler for InstantiationSet element.
 */
void
Parser
::end_InstantiationSet()
{
  // Take the InstantiationSet off the stack.
  this->PopElement();
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
  beginHandlers["Package"]            = &Parser::begin_Package;
  beginHandlers["Dependencies"]       = &Parser::begin_Dependencies;
  beginHandlers["Headers"]            = &Parser::begin_Headers;
  beginHandlers["File"]               = &Parser::begin_File;
  beginHandlers["Directory"]          = &Parser::begin_Directory;
  beginHandlers["Namespace"]          = &Parser::begin_Namespace;
  beginHandlers["Code"]               = &Parser::begin_Code;
  beginHandlers["Set"]                = &Parser::begin_Set;
  beginHandlers["Element"]            = &Parser::begin_Element;
  beginHandlers["WrapperSet"]         = &Parser::begin_WrapperSet;
  beginHandlers["InstantiationSet"]   = &Parser::begin_InstantiationSet;

  endHandlers["CableConfiguration"] = &Parser::end_CableConfiguration;
  endHandlers["Package"]            = &Parser::end_Package;
  endHandlers["Dependencies"]       = &Parser::end_Dependencies;
  endHandlers["Headers"]            = &Parser::end_Headers;
  endHandlers["File"]               = &Parser::end_File;
  endHandlers["Directory"]          = &Parser::end_Directory;
  endHandlers["Namespace"]          = &Parser::end_Namespace;
  endHandlers["Code"]               = &Parser::end_Code;
  endHandlers["Set"]                = &Parser::end_Set;
  endHandlers["Element"]            = &Parser::end_Element;
  endHandlers["WrapperSet"]         = &Parser::end_WrapperSet;
  endHandlers["InstantiationSet"]   = &Parser::end_InstantiationSet;
  
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



/**
 * A utility class to generate element combinations from all possible
 * substitutions of Set members into a $ token.
 */
class ElementCombinationGenerator
{
public:
  ElementCombinationGenerator(const Element* in_element,
                              Namespace* in_scope):
    m_Element(in_element), m_Namespace(in_scope)
    {
      this->ParseInputString();
    }
  ~ElementCombinationGenerator();
  
  void Generate(Set*);
  
private:  
  /**
   * Represent a substitution.
   */
  class Substitution
  {
  public:
    Substitution(const String& in_qualifierString):
      m_QualifierString(in_qualifierString) {}
    void Bind(const String& in_tag, const String& in_code)
      {
        m_Tag = in_tag;
        m_Code = m_QualifierString+in_code;
      }
    const String& GetTag() const
      { return m_Tag; }
    const String& GetCode() const
      { return m_Code; }
    
    /**
     * Consider an alternative qualifier string.  Since both options
     * (current m_QualifierString and given qualifier) resulted in the
     * lookup of this Substitution's set, either is acceptable.
     * Use the shorter one.
     */
    void ConsiderQualifierString(const String& qualifierString)
      {
        if(qualifierString.length() < m_QualifierString.length())
          m_QualifierString = qualifierString;
      }
    
  private:
    /**
     * The tag associated with this substitution.
     */
    String m_Tag;

    /**
     * The code to be used for the substitution.
     */
    String m_Code;

    /**
     * The namespace qualifier string used to reference the set
     * corresponding to this substitution.
     */
    String m_QualifierString;
  };
  
  
  /**
   * Interface to the parts of an input string of code, possibly with
   * $SomeSetName tokens in it.  An indivitual Portion will be either a
   * StringPortion, which has no substitutions, or a ReplacePortion, which has
   * only a substitution, and no hard-coded text.
   *
   * This is used by Parser::GenerateElementCombinations() to hold the pieces
   * of a string after the set substitution tokens have been extracted.
   */
  class Portion
  {
  public:
    /**
     * Get the C++ code corresponding to this Portion of a string.
     */
    virtual String GetCode() const =0;
    /**
     * Get the tag corresponding to this Portion of a string.  This is empty
     * for StringPortion, and holds a real tag for ReplacePortion.
     */
    virtual String GetTag() const =0;
    virtual ~Portion() {}
  };
  
  
  /**
   * Represent a hard-coded part of an input string, that has no substitutions
   * in it.  The tag for this part of a string is always empty.
   */
  class StringPortion: public Portion
  {
  public:
    StringPortion(const String& in_code): m_Code(in_code) {}
    virtual String GetCode() const
      { return m_Code; }
    virtual String GetTag() const
      { return ""; }
    virtual ~StringPortion() {}
  private:
    /**
     * Hold this Portion's contribution to the output string.
     */
    String m_Code;
  };
  

  /**
   * Represent the "$SomeSetName" portion of an input string.  This has a
   * reference to the Substitution holding the real output to generate.
   */
  class ReplacePortion: public Portion
  {
  public:
    ReplacePortion(const Substitution& in_substitution):
      m_Substitution(in_substitution) {}
    virtual String GetCode() const
      { return m_Substitution.GetCode(); }
    virtual String GetTag() const
      { return m_Substitution.GetTag(); }
    virtual ~ReplacePortion() {}
  private:
    /**
     * Refer to the real Substitution for this Portion's contribution.
     */
    const Substitution& m_Substitution;
  };
  
  typedef std::list<Portion*>  Portions;
  typedef std::map<const Set*, Substitution* >  Substitutions;
  
  /**
   * The original, unparsed element.
   */
  Element::ConstPointer m_Element;
  
  /**
   * The parts of the input string after parsing of the tokens.
   */
  Portions m_Portions;
  
  /**
   * Map from substitution's Set to actual Substitution.
   */
  Substitutions m_Substitutions;
  
  /**
   * The Namespace scope in which to begin name lookups.
   */
  Namespace::Pointer m_Namespace;
  
private:
  void Generate(Set*, Substitutions::const_iterator);
  void ParseInputString();
  String ParseSetName(String::const_iterator&, String::const_iterator) const;
};


/**
 * Destructor frees portions and substitutions that were allocated by
 * constructor.
 */
ElementCombinationGenerator
::~ElementCombinationGenerator()
{
  // Free the string portions that were allocated.
  for(Portions::iterator portion = m_Portions.begin();
      portion != m_Portions.end(); ++portion)
    {
    delete *portion;
    }
  
  // Free the substitutions that were allocated.
  for(Substitutions::iterator sub = m_Substitutions.begin();
      sub != m_Substitutions.end(); ++sub)
    {
    delete sub->second;
    }
}


/**
 * Generate all element combinations possible with the set of
 * substitutions available.  The given output set is filled with
 * all the combinations.
 */
void
ElementCombinationGenerator
::Generate(Set* out_set)
{
  // If there are no substitutions to be made, just generate this
  // single combination.
  if(m_Substitutions.empty())
    {
    out_set->Add(m_Element->GetTag(), m_Element->GetCode());
    return;
    }
  
  // We must generate all combinations of substitutions.
  // Begin the recursion with the first substitution.
  this->Generate(out_set, m_Substitutions.begin());
}


/**
 * Internal helper to Generate(Set*) which generates all combinations
 * in a recursive, depth-first order.
 */
void
ElementCombinationGenerator
::Generate(Set* out_set, Substitutions::const_iterator substitution)
{
  // Test our position in the list of substitutions to be bound.
  if(substitution == m_Substitutions.end())
    {
    // All substitutions have been prepared.  Generate this combination.
    String tag = m_Element->GetTag();
    String code = "";
    // Put together all the pieces, with substitutions.
    for(Portions::const_iterator i = m_Portions.begin();
        i != m_Portions.end(); ++i)
      {
      tag.append((*i)->GetTag());
      code.append((*i)->GetCode());
      }
    // Add this combination to the output set.
    out_set->Add(tag, code);
    }
  else
    {
    // Get the set for this substitution.
    const Set* set = substitution->first;
    if(set == out_set)
      {
      // We cannot iterate over the set currently being defined.
      throw SetSelfReferenceException(set->GetName());
      }
    
    // Prepare an iterator to the next substitution.
    Substitutions::const_iterator nextSubstitution = substitution;
    ++nextSubstitution;
    
    // We must iterate over all possible values for this substitution.
    for(Set::ConstIterator element = set->Begin();
        element != set->End(); ++element)
      {
      // Bind the substitution to this element.
      substitution->second->Bind(element->first, element->second);
      
      // Move on to the next substitution.
      this->Generate(out_set, nextSubstitution);
      }
    }
}


/**
 * Called from constructor.  Parses the input string into portions.
 * Plain text in the string is held by a StringPortion, and a $ token
 * for replacement is represented by a ReplacePortion.
 */
void
ElementCombinationGenerator
::ParseInputString()
{
  // The input code supplied in the source file.
  String in_string = m_Element->GetCode();
  
  // Break the input code into blocks alternating between literal code and
  // set-substitution tokens (like $SomeSetName).
  String currentPortion = "";
  for(String::const_iterator c=in_string.begin(); c != in_string.end(); ++c)
    {
    // Look for the '$' to mark the beginning of a token.
    if(*c != '$')
      {
      currentPortion.insert(currentPortion.end(), *c);
      }
    else
      {
      // If there is a portion of the string, record it.
      if(currentPortion.length() > 0)
        {
        m_Portions.push_back(new StringPortion(currentPortion));
        currentPortion = "";
        }
      // Skip over the '$' character.
      ++c;
      // Get element set name token.
      String setName = this->ParseSetName(c, in_string.end());

      // We have a complete set name.  Look it up in the current scope.
      String qualifierString = m_Namespace->GetQualifierString(setName);
      Set* set = m_Namespace->LookupSet(setName);
      if(set)
        {
        // We have a valid set name.  Prepare the substitution entry
        // for it.
        Substitution* sub;
        if(m_Substitutions.count(set) == 0)
          {
          sub = new Substitution(qualifierString);
          m_Substitutions[set] = sub;
          }
        else
          {
          sub = m_Substitutions[set];
          sub->ConsiderQualifierString(qualifierString);
          }
        m_Portions.push_back(new ReplacePortion(*sub));
        setName = "";
        }
      else
        {
        // Invalid set name.  Complain.
        throw UnknownSetException(setName);
        }
      
      // Let the loop look at this character again.
      --c;
      }
    }
  
  // If there is a final portion of the string, record it.
  if(currentPortion.length() > 0)
    {
    m_Portions.push_back(new StringPortion(currentPortion));
    }
}


/**
 * Parse out the name of a Set specified after a $ in the element's string.
 * This is called with "c" pointing to the first character after the $,
 * and "end" equal to the string's end iterator.
 *
 * Returns the set name after parsing.  "c" will point to the first
 * character after the end of the set name.
 */
String
ElementCombinationGenerator
::ParseSetName(String::const_iterator& c, String::const_iterator end) const
{
  String setName = "";
  
  // Check for the $(setName) syntax.
  // If the first character after the '$' is a left paren, we scan for the
  // matching paren, and take everything in-between as the set name.
  if((c != end) && (*c == '('))
    {
    unsigned int depth = 1;
    ++c;
    while(c != end)
      {
      char ch = *c++;
      if(ch == '(') { ++depth; }
      else if(ch == ')') { --depth; }
      if(depth == 0) { break; }
      setName.insert(setName.end(), ch);
      }
    return setName;
    }
  
  // The $(setName) syntax was not used.
  // Look for all characters that can be part of a qualified C++
  // identifier.
  while(c != end)
    {
    char ch = *c;
    if(((ch >= 'a') && (ch <= 'z'))
       || ((ch >= 'A') && (ch <= 'Z'))
       || ((ch >= '0') && (ch <= '9'))
       || (ch == '_') || (ch == ':'))
      {
      setName.insert(setName.end(), ch);
      ++c;
      }
    else
      {
      break;
      }
    }
  return setName;
}


/**
 * Given an Element, generate all the combinations of Set
 * substitutions possbile based on $ tokens in the Element's code.
 */
void
Parser
::GenerateElementCombinations(const Element* in_element,
                              Set* out_set) const
{
  // Create an object to handle the generation.
  ElementCombinationGenerator
    combinationGenerator(in_element, this->CurrentNamespaceScope());
  
  // Generate the combinations.
  combinationGenerator.Generate(out_set);
}


} // namespace configuration

