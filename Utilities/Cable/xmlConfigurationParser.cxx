#include "xmlConfigurationParser.h"

namespace xml
{

/**
 * Passes call through to real parser object according to first argument.
 */
static void BeginCdataSectionHandler(void* parser)
{
  static_cast<ConfigurationParser*>(parser)->BeginCdataSectionHandler();
}

/**
 * Passes call through to real parser object according to first argument.
 */
static void EndCdataSectionHandler(void* parser)
{
  static_cast<ConfigurationParser*>(parser)->EndCdataSectionHandler();
}


/**
 * Passes call through to real parser object according to first argument.
 */
static void BeginElement(void* parser, const char *name, const char **atts)
{
  static_cast<ConfigurationParser*>(parser)->BeginElement(name, atts);
}


/**
 * Passes call through to real parser object according to first argument.
 */
static void EndElement(void* parser, const char *name)
{
  static_cast<ConfigurationParser*>(parser)->EndElement(name);
}


/**
 * Passes call through to real parser object according to first argument.
 */
static void CharacterDataHandler(void* parser,const XML_Char *data, int length)
{
  static_cast<ConfigurationParser*>(parser)->CharacterDataHandler(data, length);
}


/**
 * Create a new ConfigurationParser and return a pointer to it.
 */
ConfigurationParser::Pointer
ConfigurationParser::New()
{
  return new Self;
}


/**
 * Constructor sets up an XML_Parser to make call-backs into this
 * ConfigurationParser.
 */
ConfigurationParser
::ConfigurationParser():
  m_XML_Parser(XML_ParserCreate(NULL)),
  m_Package(NULL),
  m_CdataSectionFlag(false)
{
  ConfigurationParser::InitializeHandlers();
  
  XML_SetElementHandler(m_XML_Parser,
                        ::BeginElement,
                        ::EndElement);
  XML_SetUserData(m_XML_Parser,
                  this);
  XML_SetCdataSectionHandler(m_XML_Parser,
                             ::BeginCdataSectionHandler,
                             ::EndCdataSectionHandler);
  XML_SetCharacterDataHandler(m_XML_Parser,
                              ::CharacterDataHandler);
}


/**
 * Cleanup the XML_Parser
 */
ConfigurationParser
::~ConfigurationParser()
{
  XML_ParserFree(m_XML_Parser);
}


/**
 * Parse the XML from the given input stream until end-of-input is reached.
 */
void
ConfigurationParser
::Parse(std::istream& inStream)
{
  char buf[BUFSIZ];
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
      fprintf(stderr, "ConfigurationParser::Parse(): %s at line %d\n",
              XML_ErrorString(XML_GetErrorCode(m_XML_Parser)),
              XML_GetCurrentLineNumber(m_XML_Parser));
      }
    }
}


/**
 * Called when a new element is opened in the XML source.
 * Checks the tag name, and calls the appropriate handler with an
 * Attributes container.
 */
void
ConfigurationParser
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
    e.PrintLocation(stderr);
    e.Print(stderr);
    exit(1);
    }
  catch (const String& e)
    {
    fprintf(stderr, "Caught exceptoin in BeginElement():\n%s\n", e.c_str());
    exit(1);
    }
  catch (...)
    {
    fprintf(stderr, "Caught unknown exception in BeginElement().\n");
    exit(1);
    }
}


/**
 * Called at the end of an element in the XML source opened when
 * BeginElement was called.
 */
void
ConfigurationParser
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
    e.PrintLocation(stderr);
    e.Print(stderr);
    exit(1);
    }
  catch (const String& e)
    {
    fprintf(stderr, "Caught exceptoin in EndElement():\n%s\n", e.c_str());
    exit(1);
    }
  catch (...)
    {
    fprintf(stderr, "Caught unknown exception in EndElement().\n");
    exit(1);
    }
}


/**
 * Set the flag to indicate whether we are currently parsing a CDATA
 * section from the XML source.
 */
void
ConfigurationParser
::BeginCdataSectionHandler()
{
  m_CdataSectionFlag = true;
}


/**
 * Get the flag to indicate whether we are currently parsing a CDATA
 * section from the XML source.
 */
void
ConfigurationParser
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
ConfigurationParser
::CharacterDataHandler(const XML_Char *data, int length)
{
//  ConfigurationParser* parser = static_cast<ConfigurationParser*>(in_parser);
//  if(parser->GetCdataSectionFlag())// && CurrentCodeBlock())
//    {
    //CurrentCodeBlock()->AddLine(data, length);
//    }
}


/**
 * Begin handler for Package element.
 */
void
ConfigurationParser
::begin_Package(const Attributes&)
{
//  String source = atts.Get("source");
//  String dest = "";
//  if(atts.Have("dest"))
//    dest = atts.Get("dest");
  m_Package = Package::New();
}

/**
 * End handler for Package element.
 */
void
ConfigurationParser
::end_Package()
{
}


/**
 * Map of ConfigurationParser element begin handlers.
 */
ConfigurationParser::BeginHandlers ConfigurationParser::beginHandlers;

/**
 * Map of ConfigurationParser element end handlers.
 */
ConfigurationParser::EndHandlers ConfigurationParser::endHandlers;

/**
 * This static method initializes the beginHandlers and endHandlers
 * maps.  It is called by the ConfigurationParser constructor every time,
 * but only initializes the maps once.
 */
void
ConfigurationParser
::InitializeHandlers(void)
{
  // Make sure we only initialize the maps once.
  static bool initialized = false;  
  if(initialized) return;
  
  beginHandlers["Package"] = &ConfigurationParser::begin_Package;
  endHandlers["Package"]   = &ConfigurationParser::end_Package;
  
  initialized = true;
}


#if 0


/**
 * A stack of code block definitions.
 */
static std::stack<CodeBlock::Pointer>  codeBlockStack;



/**
 * Begin handler for WrapType element.
 */
void
ConfigurationParser
::begin_WrapType(const Attributes& atts)
{
  String name = atts.Get("name");
  WrapType::Pointer newWrapType = WrapType::New(name);
  
  wrapperConfiguration->AddWrapType(newWrapType);
  currentWrapType = newWrapType;
}

/**
 * End handler for WrapType element.
 */
void
ConfigurationParser
::end_WrapType(void)
{
  // If there was no creation, use the current default.
  if(!currentWrapType->GetCreate())
    {
    currentWrapType->SetCreate(wrapperConfiguration->GetDefaultCreate());
    }
  // If there was no deletion, use the current default.
  if(!currentWrapType->GetDelete())
    {
    currentWrapType->SetDelete(wrapperConfiguration->GetDefaultDelete());
    }
  // No wrapping type is currently being defined.
  currentWrapType = NULL;
}


/**
 * Begin handler for DefaultCreate element.
 */
void
ConfigurationParser
::begin_DefaultCreate(const Attributes&)
{
  Create::Pointer newCreate = Create::New();
  
  wrapperConfiguration->SetDefaultCreate(newCreate);
  PushCodeBlock(newCreate);
}

/**
 * End handler for DefaultCreate element.
 */
void
ConfigurationParser
::end_DefaultCreate(void)
{
  // Remove the current default creation setting.
  wrapperConfiguration->SetDefaultCreate(NULL);
  PopCodeBlock();
}


/**
 * Begin handler for DefaultDelete element.
 */
void
ConfigurationParser
::begin_DefaultDelete(const Attributes&)
{
  Delete::Pointer newDelete = Delete::New();
  
  wrapperConfiguration->SetDefaultDelete(newDelete);
  PushCodeBlock(newDelete);
}

/**
 * End handler for DefaultDelete element.
 */
void
ConfigurationParser
::end_DefaultDelete(void)
{
  // Remove the current default deletion setting.
  wrapperConfiguration->SetDefaultDelete(NULL);
  PopCodeBlock();
}


/**
 * Begin handler for Create element.
 */
void
ConfigurationParser
::begin_Create(const Attributes&)
{
  Create::Pointer newCreate = Create::New();
  
  if(!currentWrapType) throw "Create setting without WrapType.\n";
  currentWrapType->SetCreate(newCreate);
  PushCodeBlock(newCreate);
}

/**
 * End handler for Create element.
 */
void
ConfigurationParser
::end_Create(void)
{
  PopCodeBlock();
}


/**
 * Begin handler for Delete element.
 */
void
ConfigurationParser
::begin_Delete(const Attributes&)
{
  Delete::Pointer newDelete = Delete::New();
  
  if(!currentWrapType) throw "Delete setting without WrapType.\n";
  currentWrapType->SetDelete(newDelete);
  PushCodeBlock(newDelete);
}

/**
 * End handler for Delete element.
 */
void
ConfigurationParser
::end_Delete(void)
{
  PopCodeBlock();
}

/**
 * Begin a new code block.
 */
static void PushCodeBlock(CodeBlock* c)
{
  codeBlockStack.push(c);
}


/**
 * End a code block.
 */
static void PopCodeBlock(void)
{
  codeBlockStack.pop();
}


/**
 * The current code block.
 */
static CodeBlock::Pointer CurrentCodeBlock(void)
{
  if(codeBlockStack.empty()) return NULL;
  else return codeBlockStack.top();
}



#endif

} // namespace xml
