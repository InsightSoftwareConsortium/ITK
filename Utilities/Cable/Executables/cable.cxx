#include <stdio.h>
#include <stdlib.h>

#include "parseConfigXML.h"
#include "parseSourceXML.h"

extern void generateTcl(Namespace* globalNamespace);
extern void displayTree(Namespace* globalNamespace);


/**
 * A structure to define a wrapper generator.
 */
struct WrapperGenerator
{
  char* languageName;            // Name of wrapping language.
  char* commandLineFlag;         // Command line flag's text.
  bool  flag;                    // Was command line flag given?
  void (*generate)(Namespace*);  // Generation function.
};


/**
 * An array of the wrapper generators defined.
 */
WrapperGenerator wrapperGenerators[] =
{
  { "TCL", "-tcl", false, generateTcl},
  { "(display tree)", "-display", false, displayTree},
  { 0, 0, 0, 0 }
};


/**
 * The input file to be used (defaults to stdin).
 */
FILE* inputFile = NULL;

/**
 * The configuration file to be used (default none).
 */
FILE* configFile = NULL;

void processCommandLine(int argc, char* argv[]);

/**
 * Program entry point.
 */
int main(int argc, char* argv[]) try
{
  processCommandLine(argc, argv);

  // Store the configuration.
  Configuration::Pointer configuration;
  
  if(configFile)
    {
    // Parse the configuration XML input.
    configuration = ParseConfigXML(configFile);
    }
  else
    {
    // Use the default configuration.
    configuration = Configuration::New();
    }
  
  /**
   * Parse the source XML input.
   */
  Namespace::Pointer globalNamespace = ParseSourceXML(inputFile);
  
  /**
   * Generate all wrappers requested.
   */
  for(WrapperGenerator* wrapperGenerator = wrapperGenerators;
      wrapperGenerator->languageName;
      ++wrapperGenerator)
    {
    if(wrapperGenerator->flag)
      {
      printf("Generating %s wrappers...\n", wrapperGenerator->languageName);
      wrapperGenerator->generate(globalNamespace);
      }
    }
       
  printf("Done.\n");
  
  if(inputFile) fclose(inputFile);
  if(configFile) fclose(configFile);

return 0;
}
catch(String s)
{
  fprintf(stderr, "main(): Caught exception: %s\n", s.c_str());
  exit(1);
}
catch(char* s)
{
  fprintf(stderr, "main(): Caught exception: %s\n", s);
  exit(1);
}
catch(...)
{
  fprintf(stderr, "main(): Caught unknown exception!\n");
  exit(1);
}


/**
 * Loop through all the arguments.  Any unrecognized argument
 * is assumed to be an input file name.
 */
void processCommandLine(int argc, char* argv[])
{
  int curArg;
  
  for(curArg = 1; curArg < argc ; ++curArg)
    {
    
    /**
     * See if the option specifies any wrapping languages.
     */
    bool found = false;
    for(WrapperGenerator* wrapperGenerator = wrapperGenerators;
        wrapperGenerator->languageName;
        ++wrapperGenerator)
      {
      if(strcmp(wrapperGenerator->commandLineFlag, argv[curArg]) == 0)
        {
        found = true;
        wrapperGenerator->flag = true;
        break;
        }
      }

    /**
     * Check if this option specifies a wrapper configuration file.
     */
    if(!found && (strcmp("-config", argv[curArg]) == 0))
      {
      found = true;
      if(++curArg >= argc) break;
      configFile = fopen(argv[curArg], "rt");
      if(!configFile)
        {
        fprintf(stderr, "Error opening config file: %s\n", argv[curArg]);
        exit(1);
        }
      printf("Using configuration file: %s\n", argv[curArg]);
      }
    
    /**
     * Assume the option specifies input.
     */
    if(!found)
      {
      inputFile = fopen(argv[curArg], "rt");
      if(!inputFile)
        {
        fprintf(stderr, "Error opening input file: %s\n", argv[curArg]);
        exit(1);
        }
      }
    }

  if(!inputFile)
    {
    printf("Using standard input.\n");
    inputFile = stdin;
    }
  
}

