#include <stack>
#include <cstdlib>
#include "parseConfigXML.h"

/**
 * Map from element name to its beginning handler.
 */
static std::map<String, void (*)(const Attributes&)>  beginHandlers;

/**
 * Map from element name to its ending handler.
 */
static std::map<String, void (*)(void)>  endHandlers;

/**
 * A stack of code block definitions.
 */
static std::stack<CodeBlock::Pointer>  codeBlockStack;

/**
 * Flag for whether a CDATA section is being parsed.
 */
static bool in_CDATA_Section = false;

/**
 * Current wrap type definition.
 */
static WrapType::Pointer currentWrapType;

/**
 * The wrapper configuration.
 */
WrapperConfiguration::Pointer wrapperConfiguration;

static void Initialize(void);
static void StartElement(void *, const char *, const char **);
static void EndElement(void *, const char *);
static void CharacterDataHandler(void *,const XML_Char *, int);
static void StartCdataSectionHandler(void*) { in_CDATA_Section = true; }
static void EndCdataSectionHandler(void*) { in_CDATA_Section = false; }

WrapperConfiguration::Pointer ParseConfigXML(FILE* inFile)
{
  /**
   * Prepare the XML parser.
   */
  char buf[BUFSIZ];
  XML_Parser parser = XML_ParserCreate(NULL);
  bool done = false;
  
  Initialize();

  XML_SetElementHandler(parser, StartElement, EndElement);
  XML_SetCdataSectionHandler(parser, StartCdataSectionHandler,
                             EndCdataSectionHandler);
  XML_SetCharacterDataHandler(parser, CharacterDataHandler);
    
  /**
   * Parse the entire XML source.
   */
  while(!done)
    {
    size_t len = fread(buf, 1, sizeof(buf), inFile);
    done = len < sizeof(buf);
    if (!XML_Parse(parser, buf, len, done))
      {
      fprintf(stderr, "ParseConfigXML(): %s at line %d\n",
              XML_ErrorString(XML_GetErrorCode(parser)),
              XML_GetCurrentLineNumber(parser));
      exit(1);
      }
    }

  /**
   * Done with the parser.
   */
  XML_ParserFree(parser);

  fclose(inFile);
  
  return wrapperConfiguration;
}


/**
 * Called when a new element is opened in the XML source.
 * Checks the tag name, and calls the appropriate handler with an
 * Attributes container.
 */
void StartElement(void *, const char *name, const char **atts)
{
  try {
  if(beginHandlers.count(name) > 0)
    {
    Attributes attributes;
    for(int i=0; atts[i] && atts[i+1] ; i += 2)
      {
      attributes.Set(atts[i], atts[i+1]);
      }
    beginHandlers[name](attributes);
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
    fprintf(stderr, "Caught exceptoin in StartElement():\n%s\n", e.c_str());
    exit(1);
    }
  catch (...)
    {
    fprintf(stderr, "Caught unknown exception in StartElement().\n");
    exit(1);
    }
}


/**
 * Called at the end of an element in the XML source opened when
 * StartElement was called.
 */
void EndElement(void *, const char *name)
{
  try {
  if(endHandlers.count(name) > 0)
    {
    endHandlers[name]();
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


/**
 * When an XML character data segment is encountered, this is called
 * to handle it.
 *
 * Such a segment is of the form:
 * <![CDATA[....]]>
 * "data" will be ...., one line at a time.
 * "data" will NOT be '\0' terminated, but "length" specifies how much.
 */
void CharacterDataHandler(void *,const XML_Char *data, int length)
{
  if(in_CDATA_Section && CurrentCodeBlock())
    {
    CurrentCodeBlock()->AddLine(data, length);
    }
}


/**
 * Begin handler for WrapperConfiguration element.
 */
static void begin_WrapperConfiguration(const Attributes& atts)
{
  String source = atts.Get("source");
  String dest = "";
  if(atts.Have("dest"))
    dest = atts.Get("dest");
  wrapperConfiguration = WrapperConfiguration::New(source, dest);
}

/**
 * End handler for WrapperConfiguration element.
 */
static void end_WrapperConfiguration(void)
{
}


/**
 * Begin handler for WrapType element.
 */
static void begin_WrapType(const Attributes& atts)
{
  String name = atts.Get("name");
  WrapType::Pointer newWrapType = WrapType::New(name);
  
  wrapperConfiguration->AddWrapType(newWrapType);
  currentWrapType = newWrapType;
}

/**
 * End handler for WrapType element.
 */
static void end_WrapType(void)
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
static void begin_DefaultCreate(const Attributes&)
{
  Create::Pointer newCreate = Create::New();
  
  wrapperConfiguration->SetDefaultCreate(newCreate);
  PushCodeBlock(newCreate);
}

/**
 * End handler for DefaultCreate element.
 */
static void end_DefaultCreate(void)
{
  // Remove the current default creation setting.
  wrapperConfiguration->SetDefaultCreate(NULL);
  PopCodeBlock();
}


/**
 * Begin handler for DefaultDelete element.
 */
static void begin_DefaultDelete(const Attributes&)
{
  Delete::Pointer newDelete = Delete::New();
  
  wrapperConfiguration->SetDefaultDelete(newDelete);
  PushCodeBlock(newDelete);
}

/**
 * End handler for DefaultDelete element.
 */
static void end_DefaultDelete(void)
{
  // Remove the current default deletion setting.
  wrapperConfiguration->SetDefaultDelete(NULL);
  PopCodeBlock();
}


/**
 * Begin handler for Create element.
 */
static void begin_Create(const Attributes&)
{
  Create::Pointer newCreate = Create::New();
  
  if(!currentWrapType) throw "Create setting without WrapType.\n";
  currentWrapType->SetCreate(newCreate);
  PushCodeBlock(newCreate);
}

/**
 * End handler for Create element.
 */
static void end_Create(void)
{
  PopCodeBlock();
}


/**
 * Begin handler for Delete element.
 */
static void begin_Delete(const Attributes&)
{
  Delete::Pointer newDelete = Delete::New();
  
  if(!currentWrapType) throw "Delete setting without WrapType.\n";
  currentWrapType->SetDelete(newDelete);
  PushCodeBlock(newDelete);
}

/**
 * End handler for Delete element.
 */
static void end_Delete(void)
{
  PopCodeBlock();
}


void Initialize(void)
{
  beginHandlers["WrapperConfiguration"] = begin_WrapperConfiguration;
  beginHandlers["WrapType"]             = begin_WrapType;
  beginHandlers["DefaultCreate"]        = begin_DefaultCreate;
  beginHandlers["DefaultDelete"]        = begin_DefaultDelete;
  beginHandlers["Create"]               = begin_Create;
  beginHandlers["Delete"]               = begin_Delete;

  endHandlers["WrapperConfiguration"] = end_WrapperConfiguration;
  endHandlers["WrapType"]             = end_WrapType;
  endHandlers["DefaultCreate"]        = end_DefaultCreate;
  endHandlers["DefaultDelete"]        = end_DefaultDelete;
  endHandlers["Create"]               = end_Create;
  endHandlers["Delete"]               = end_Delete;
}
