#include <stdio.h>
#include <stdlib.h>

#include "parseConfigXML.h"
#include "parseSourceXML.h"

extern void generateTcl(Namespace* globalNamespace, WrapperConfiguration*);
extern void displayTree(Namespace* globalNamespace, WrapperConfiguration*);


/**
 * A structure to define a wrapper generator.
 */
struct WrapperGenerator
{
  char* languageName;            // Name of wrapping language.
  char* commandLineFlag;         // Command line flag's text.
  bool  flag;                    // Was command line flag given?
  void (*generate)(Namespace*, WrapperConfiguration*);  // Generation function.
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
 * The input file to be used for configuration (defaults to stdin).
 */
FILE* inputFile = NULL;

/**
 * If the command line override's the configuration file's output name,
 * this is set to the new file name.
 */
char* outputFile = NULL;

void processCommandLine(int argc, char* argv[]);

/**
 * Program entry point.
 */
int main(int argc, char* argv[])
{
  try {
  processCommandLine(argc, argv);
  
  // Store the configuration.
  WrapperConfiguration::Pointer configuration =
    ParseConfigXML(inputFile);

  // If needed, override output file name.
  if(outputFile)
    {
    configuration->SetOutputFile(outputFile);
    }
  
  // Parse the source XML input.
  Namespace::Pointer globalNamespace =
    ParseSourceXML(configuration->GetSourceXML());

  // Make sure all the types requested were in the translation unit.
  if(!configuration->FindTypes(globalNamespace))
    {
    fprintf(stderr, "Not all types defined in namespace...these are missing:\n");
    configuration->PrintMissingTypes(stderr);
    exit(1);
    }
  
  // Generate all wrappers requested.
  for(WrapperGenerator* wrapperGenerator = wrapperGenerators;
      wrapperGenerator->languageName;
      ++wrapperGenerator)
    {
    if(wrapperGenerator->flag)
      {
      fprintf(stderr, "Generating %s wrappers...\n",
              wrapperGenerator->languageName);
      wrapperGenerator->generate(globalNamespace, configuration);
      }
    }
  fprintf(stderr, "Done.\n");
  }
  catch(String s)
    {
    fprintf(stderr, "main(): Caught exception:\n%s\n", s.c_str());
    return 1;
    }
  catch(char* s)
    {
    fprintf(stderr, "main(): Caught exception:\n%s\n", s);
    return 1;
    }
  catch(...)
    {
    fprintf(stderr, "main(): Caught unknown exception!\n");
    return 1;
    }
  
return 0;
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

    if(strcmp("-o", argv[curArg]) == 0)
      {
      found = true;
      outputFile = argv[++curArg];
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
    fprintf(stderr, "Using standard input.\n");
    inputFile = stdin;
    }
}

