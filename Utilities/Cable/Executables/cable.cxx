/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cable.cxx
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
#include "cableSourceParser.h"

#include "genCxxGenerator.h"
#include "genTclGenerator.h"

#include <iostream>
#include <fstream>

typedef configuration::CableConfiguration  CableConfiguration;
typedef source::Namespace       Namespace;

/**
 * A structure to define a wrapper generator.
 */
struct WrapperGenerator
{
  char* languageName;            // Name of wrapping language.
  char* commandLineFlag;         // Command line flag's text.
  bool  flag;                    // Was command line flag given?
  gen::GeneratorBase* (*get)(const CableConfiguration*,
                             const Namespace*); // Get a generator.
};


/**
 * An array of the wrapper generators defined.
 */
WrapperGenerator wrapperGenerators[] =
{
  { "Tcl", "-tcl", false, &gen::TclGenerator::GetInstance },
  { 0, 0, 0, 0 }
};


/**
 * The input stream to be used for configuration.
 */
std::ifstream configFile;

/**
 * The input stream to be used for XML source representation.
 * This is specified with the -source option.
 */
std::ifstream sourceFile;

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
  
  configurationParser->Parse(configFile);
  const CableConfiguration* cableConfiguration =
    configurationParser->GetCableConfiguration();

  if(!sourceFile.is_open())
    {
    gen::GeneratorBase* cxxGenerator =
      gen::CxxGenerator::GetInstance(cableConfiguration);
    
    cxxGenerator->Generate();
    
    delete cxxGenerator;
    }
  else
    {
    source::Parser::Pointer sourceParser = source::Parser::New();  
    sourceParser->Parse(sourceFile);
    
    const Namespace* globalNamespace = sourceParser->GetGlobalNamespace();
    
    // Generate all wrappers requested.
    for(WrapperGenerator* wrapperGenerator = wrapperGenerators;
        wrapperGenerator->languageName; ++wrapperGenerator)
      {
      if(wrapperGenerator->flag)
        {
        std::cout << "Generating " << wrapperGenerator->languageName
                  << " wrappers..." << std::endl;
        gen::GeneratorBase* generator =
          (wrapperGenerator->get)(cableConfiguration, globalNamespace);
        generator->Generate();
        delete generator;
        }
      }
    std::cout << "Done";
    }
  }
  catch(String s)
    {
    std::cerr << "main(): Caught exception: " << std::endl
              << s.c_str() << std::endl;
    return 1;
    }
  catch(char* s)
    {
    std::cerr << "main(): Caught exception: " << std::endl
              << s << std::endl;
    return 1;
    }
  catch(...)
    {
    std::cerr << "main(): Caught unknown exception!" << std::endl;
    return 1;
    }
  
return 0;
}


/**
 * Loop through all the arguments.  Any unrecognized argument
 * is assumed to be an config file name.
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

    if(strcmp("-source", argv[curArg]) == 0)
      {
      found = true;
      sourceFile.open(argv[++curArg]);
      if(!sourceFile)
        {
        std::cerr << "Error opening source file: " << argv[curArg] << std::endl;
        return false;
        }
      }
    
    /**
     * Assume the option specifies config.
     */
    if(!found)
      {
      configFile.open(argv[curArg]);
      if(!configFile)
        {
        std::cerr << "Error opening config file: " << argv[curArg] << std::endl;
        return false;
        }
      }
    }
  
  if(!configFile)
    {
    std::cerr << "No config file specified!" << std::endl;
    return false;
    }
  
  return true;
}

