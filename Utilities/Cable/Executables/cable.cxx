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
typedef configuration::Package  Package;
typedef source::Namespace       Namespace;
  
/**
 * A class to define a wrapper generator.
 */
class WrapperGenerator
{
public:
  typedef gen::GeneratorBase* (*AllocateFunction)(const Package*, const Namespace*);
  WrapperGenerator(const String& languageName,
                   const String& commandLineFlag,
                   AllocateFunction get):
    m_LanguageName(languageName), m_CommandLineFlag(commandLineFlag),
    m_Get(get), m_Enabled(false) {}
  
  void Enable() { m_Enabled = true; }
  void Disable() { m_Enabled = false; }
  bool Enabled() const { return m_Enabled; }
  const String& GetLanguageName() const { return m_LanguageName; }
  const String& GetCommandLineFlag() const { return m_CommandLineFlag; }
  gen::GeneratorBase* GetGenerator(const Package* p,
                                   const Namespace* ns)
    {
      if(m_Get) { return m_Get(p, ns); }
      else { return NULL; }
    }
  
private:
  String m_LanguageName;
  String m_CommandLineFlag;
  AllocateFunction m_Get;
  bool m_Enabled;
};


class Cable
{
public:
  Cable(): m_GeneratingCxx(true) {}
  void Add(const WrapperGenerator&);
  bool ProcessCommandLine(int argc, char* argv[]);
  bool ParseConfiguration();
  void Generate();
private:
  void GeneratePackageCxx(const Package*);
  void GeneratePackageWrappers(const Package*);
  std::vector<WrapperGenerator> m_WrapperGenerators;
  String m_ConfigurationFileName;
  CableConfiguration::ConstPointer m_CableConfiguration;
  std::set<String> m_Packages;
  bool m_GeneratingCxx;
};

void Cable::Add(const WrapperGenerator& wg)
{
  m_WrapperGenerators.push_back(wg);
}

/**
 * Loop through all the arguments.  Any unrecognized argument
 * is assumed to be an config file name.
 */
bool Cable::ProcessCommandLine(int argc, char* argv[])
{
  std::vector<String> arguments;
  for(int i=1; i < argc; ++i)
    {
    arguments.push_back(argv[i]);
    }

  bool forceCxx = false;
  bool haveConfig = false;
  
  for(std::vector<String>::const_iterator arg = arguments.begin();
      arg != arguments.end(); ++arg)
    {
    if(!forceCxx)
      {
      bool found = false;
      for(std::vector<WrapperGenerator>::iterator wg = m_WrapperGenerators.begin();
          wg != m_WrapperGenerators.end(); ++wg)
      {
      if(*arg == wg->GetCommandLineFlag())
        {
        wg->Enable();
        m_GeneratingCxx = false;
        found = true;
        }
      }
      if(found) { continue; }
      }
    if(*arg == "-package")
      {
      ++arg;
      if(arg == arguments.end())
        {
        std::cerr << "  Command line argument -package given with no value." << std::endl;
        return false;
        }
      m_Packages.insert(*arg);
      std::cout << "  Enabling generation for package \"" << arg->c_str() << "\"" << std::endl;
      }
    else if(*arg == "-cxx")
      {
      m_GeneratingCxx = true;
      forceCxx = true;
      std::cout << "  Command line argument -cxx given.  Ignoring other languages." << std::endl;
      }
    else if(!haveConfig)
      {
      m_ConfigurationFileName = *arg;
      haveConfig = true;
      }
    else
      {
      std::cerr << "  Unknown command-line argument: " << arg->c_str() << std::endl;
      std::cout << "  Configuration file already specified as \"" << m_ConfigurationFileName.c_str() << "\"" << std::endl;
      return false;
      }
    }
  
  if(!haveConfig)
    {
    std::cerr << "  No config file specified!" << std::endl;
    return false;
    }
  
  return true;
}

bool Cable::ParseConfiguration()
{
  configuration::Parser::Pointer configurationParser = configuration::Parser::New();
  
  std::ifstream configFile(m_ConfigurationFileName.c_str());
  if(!configFile)
    {
    std::cerr << "  Error opening configuration file \""
              << m_ConfigurationFileName.c_str() << "\"" << std::endl;
    return false;
    }
  
  configurationParser->Parse(configFile);
  m_CableConfiguration = configurationParser->GetCableConfiguration();
  
  return true;
}

void Cable::Generate()
{
  // Loop over all pacakges in the configuration.
  for(CableConfiguration::PackageIterator package = m_CableConfiguration->BeginPackages();
      package != m_CableConfiguration->EndPackages(); ++package)
    {
    if(m_Packages.empty() || (m_Packages.find((*package)->GetName()) != m_Packages.end()))
      {
      if(m_GeneratingCxx)
        {
        this->GeneratePackageCxx(*package);
        }
      else
        {
        this->GeneratePackageWrappers(*package);
        }
      }
    }
}

void Cable::GeneratePackageCxx(const Package* package)
{
  gen::GeneratorBase* cxxGenerator = gen::CxxGenerator::GetInstance(package);  
  cxxGenerator->Generate();
  delete cxxGenerator;
}

void Cable::GeneratePackageWrappers(const Package* package)
{
  std::cout << "  Generating package " << package->GetName().c_str() << std::endl;
  String sourceName = "Cxx/"+package->GetName()+"_cxx.xml";
  
  std::ifstream sourceFile(sourceName.c_str());
  if(!sourceFile)
    {
    std::cerr << "    Error opening XML source file: \"" << sourceName.c_str() << "\"" << std::endl
              << "    Skipping this package." << std::endl;
    return;
    }
  
  source::Parser::Pointer sourceParser = source::Parser::New();  
  sourceParser->Parse(sourceFile);
  const Namespace* globalNamespace = sourceParser->GetGlobalNamespace();
  
  for(std::vector<WrapperGenerator>::iterator
        wg = m_WrapperGenerators.begin();
      wg != m_WrapperGenerators.end(); ++wg)
    {
    if(wg->Enabled())
      {
      std::cout << "    Generating " << wg->GetLanguageName().c_str()
                << " wrappers..." << std::endl;
      gen::GeneratorBase* generator =
        wg->GetGenerator(package, globalNamespace);
      if(generator)
        {
        generator->Generate();
        delete generator;
        }
      else
        {
        std::cerr << "      Error creating wrapper generation class.  Disabling." << std::endl;
        wg->Disable();
        }
      }
    }
}


/**
 * Program entry point.
 */
int main(int argc, char* argv[])
{
  Cable cable;
  cable.Add(WrapperGenerator("Tcl", "-tcl", &gen::TclGenerator::GetInstance));
  
  try {
  if(!cable.ProcessCommandLine(argc, argv))
    { return 1; }
  if(!cable.ParseConfiguration())
    { return 1; }
  cable.Generate();
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
