#include <iostream>
#include <fstream>

#include "configurationParser.h"
#include "sourceParser.h"

typedef std::string String;
typedef source::Namespace       Namespace;
typedef configuration::Package  Package;

//extern void GenerateTcl(const Namespace* globalNamespace,
//                        const Package*,
//                        const char* outputDirectory);
extern void DisplayTree(const Namespace* globalNamespace,
                        const Package*,
                        const char* outputDirectory);


/**
 * A structure to define a wrapper generator.
 */
struct WrapperGenerator
{
  char* languageName;            // Name of wrapping language.
  char* commandLineFlag;         // Command line flag's text.
  bool  flag;                    // Was command line flag given?
  void (*generate)(const Namespace*,
                   const Package*,
                   const char*); // Generation function.
  char* outputDirectory;         // Name of subdirectory where wrappers go.
};


/**
 * An array of the wrapper generators defined.
 */
WrapperGenerator wrapperGenerators[] =
{
//  { "TCL", "-tcl", false, GenerateTcl, "Tcl"},
  { "(display tree)", "-display", false, DisplayTree, ""},
  { 0, 0, 0, 0 }
};


/**
 * The input stream to be used for configuration.
 */
std::ifstream inputFile;

/**
 * If the command line override's the configuration file's output name,
 * this is set to the new file name.
 */
const char* outputName = NULL;

bool processCommandLine(int argc, char* argv[]);

/**
 * Program entry point.
 */
int main(int argc, char* argv[])
{
  try {
  if(!processCommandLine(argc, argv)) return 1;
 
  configuration::Parser::Pointer configurationParser =
    configuration::Parser::New();
  
  configurationParser->Parse(inputFile);
  
//  source::Parser::Pointer sourceParser = source::Parser::New();
  
//  sourceParser->Parse(inputFile);
  
#if 0
  // If needed, override output file name.
  if(outputName)
    {
    configuration->SetOutputName(outputName);
    }
  
  // Parse the source XML input.
  Namespace::Pointer globalNamespace =
    ParseSourceXML(configuration->GetSourceXML());

  // Make sure all the types requested were in the translation unit.
  if(!configuration->FindTypes(globalNamespace))
    {
    fprintf(stderr, "Not all types defined in source...these are missing:\n");
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
      wrapperGenerator->generate(globalNamespace, configuration,
                                 wrapperGenerator->outputDirectory);
      }
    }
  fprintf(stderr, "Done.\n");
#endif  
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
bool processCommandLine(int argc, char* argv[])
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
      outputName = argv[++curArg];
      }
    
    /**
     * Assume the option specifies input.
     */
    if(!found)
      {
      inputFile.open(argv[curArg]);
      if(!inputFile)
        {
        std::cerr << "Error opening input file: " << argv[curArg] << std::endl;
        return false;
        }
      }
    }
  
  if(!inputFile)
    {
    std::cerr << "No input file specified!" << std::endl;
    return false;
    }
  
  return true;
}

