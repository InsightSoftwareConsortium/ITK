#include <cstdio>
#include <cstdlib>
#include "parseUtils.h"


/**
 * Called when a new element is opened in the XML source.
 * Checks the tag name, and calls the appropriate handler with an
 * Attributes container.
 */
void StartElement(void *userData, const char *name, const char **atts) try
{
  BeginHandlers& beginHandlers = *(((HandlersPair*)userData)->beginHandlers);
  
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


/**
 * Called at the end of an element in the XML source opened when
 * StartElement was called.
 */
void EndElement(void *userData, const char *name) try
{
  EndHandlers& endHandlers = *(((HandlersPair*)userData)->endHandlers);

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

