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
  m_Package(NULL),
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
    throw UnknownElementTagException(__FILE__, __LINE__, name);
    }
  }
  catch (const ParseException& e)
    {
    e.PrintLocation(std::cerr);
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
    throw UnknownElementTagException(__FILE__, __LINE__, name);
    }
  }
  catch (const ParseException& e)
    {
    e.PrintLocation(std::cerr);
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
 * "data" will be ...., one line at a time.
 * "data" will NOT be '\0' terminated, but "length" specifies how much.
 */
void
Parser
::CharacterDataHandler(const XML_Char *data, int length)
{
//  Parser* parser = static_cast<Parser*>(in_parser);
//  if(parser->GetCdataSectionFlag())// && CurrentCodeBlock())
//    {
    //CurrentCodeBlock()->AddLine(data, length);
//    }
}


/**
 * Get the top of the element stack.
 */
ConfigureObject::Pointer
Parser
::CurrentElement(void)
{
  return m_ElementStack.top();
}


/**
 * An unexpected element type is on top of the stack.
 */
class ElementStackTypeException: public ParseException
{
public:
  ElementStackTypeException(const char* file, int line,
                            const char* e, const char* t):
    ParseException(file, line), m_Expected(e), m_Got(t) {}
  void Print(std::ostream& os) const
    {
      os << "Expected \"" << m_Expected.c_str()
         << "\" on top of element stack, but got \"" << m_Got << "\""
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
::CurrentPackage(void)
{
  if(m_ElementStack.top()->GetTypeOfObject() != Package_id)
    throw ElementStackTypeException(__FILE__, __LINE__, "Package",
                                    m_ElementStack.top()->GetClassName());

  return dynamic_cast<Package*>(m_ElementStack.top().RealPointer());
}


/**
 * Get the current Dependencies off the top of the element stack.
 */
Dependencies::Pointer
Parser
::CurrentDependencies(void)
{
  if(m_ElementStack.top()->GetTypeOfObject() != Dependencies_id)
    throw ElementStackTypeException(__FILE__, __LINE__, "Dependencies",
                                    m_ElementStack.top()->GetClassName());

  return dynamic_cast<Dependencies*>(m_ElementStack.top().RealPointer());
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
::PopElement(void)
{
  m_ElementStack.pop();

  // Sanity check.
  if(m_ElementStack.empty())
    {
    throw String("Main package popped from element stack!");
    }
}


/**
 * Begin handler for Package element.
 */
void
Parser
::begin_Package(const Attributes& atts)
{
  String name = atts.Get("name");
  if(!m_Package)
    {
    // This is the outermost package.
    m_Package = Package::New(name);
    this->PushElement(m_Package);
    }
  else
    {
    // This is a package dependency element.
    // Add the dependency.
    this->CurrentDependencies()->Add(name);
    }
}

/**
 * End handler for Package element.
 */
void
Parser
::end_Package()
{
  // Don't pop off the main package element!
}


/**
 * Begin handler for Dependencies element.
 */
void
Parser
::begin_Dependencies(const Attributes&)
{
  Package* package = this->CurrentPackage();
  Dependencies::Pointer dependencies = package->GetDependencies();
  if(!dependencies)
    {
    dependencies = Dependencies::New();
    package->SetDependencies(dependencies);
    }
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
::InitializeHandlers(void)
{
  // Make sure we only initialize the maps once.
  static bool initialized = false;  
  if(initialized) return;
  
  beginHandlers["Package"]      = &Parser::begin_Package;
  beginHandlers["Dependencies"] = &Parser::begin_Dependencies;

  endHandlers["Package"]      = &Parser::end_Package;
  endHandlers["Dependencies"] = &Parser::end_Dependencies;
  
  initialized = true;
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


} // namespace configuration
