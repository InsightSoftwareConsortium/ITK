#include <cstdlib>
#include "parseConfigXML.h"


/**
 * Begin handlers map.
 */
static BeginHandlers beginHandlers;

/**
 * End handlers map.
 */
static EndHandlers endHandlers;

static void Initialize(void);

Configuration::Pointer ParseConfigXML(FILE* inFile)
{
  if(!inFile) return NULL;
  
  /**
   * Prepare the XML parser.
   */
  char buf[BUFSIZ];
  XML_Parser parser = XML_ParserCreate(NULL);
  bool done = false;
  
  Initialize();

  HandlersPair hp(&beginHandlers, &endHandlers);
  
  XML_SetUserData(parser, &hp);
  XML_SetElementHandler(parser, StartElement, EndElement);

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
}

void Initialize(void)
{
}
