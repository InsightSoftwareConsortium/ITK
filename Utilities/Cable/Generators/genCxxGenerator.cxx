/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    genCxxGenerator.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "genCxxGenerator.h"

#include <stack>
#include <iostream>
#include <fstream>

namespace gen
{

using namespace configuration;


/**
 * Construct an instance of this generator and return it.
 */
GeneratorBase* CxxGenerator::GetInstance(const CableConfiguration* in_config)
{
  return new CxxGenerator(in_config);
}


/**
 * Generate C++ wrappers for all packages specified in the configuration.
 */
void
CxxGenerator
::Generate()
{
  // Just loop over all pacakges in the configuration.
  for(CableConfiguration::PackageIterator package =
        m_CableConfiguration->BeginPackages();
      package != m_CableConfiguration->EndPackages(); ++package)
    {
    this->GeneratePackage(*package);
    }
}
 

/**
 * Generate the C++ wrappers for the given package.
 */
void
CxxGenerator
::GeneratePackage(const Package* package)
{
  // Make sure the output directory exists.
  if(!GeneratorBase::MakeDirectory("Cxx"))
    {
    std::cerr << "Error making Cxx directory." << std::endl;
    return;
    }
  
  // Setup the output file names.
  String wrapperFile = "Cxx/"+package->GetName()+"_cxx.h";
  String instantiationFile = "Cxx/"+package->GetName()+"_cxx.cxx";
  
  // Open the output files.
  std::ofstream wrapperStream(wrapperFile.c_str());
  if(!wrapperStream) { return; }
  std::ofstream instantiationStream(instantiationFile.c_str());
  if(!instantiationStream) { return; }
  
  // Write the standard #ifndef/#define pair in the header file.
  wrapperStream << "#ifndef _" << package->GetName() << "_cxx_h" << std::endl;
  wrapperStream << "#define _" << package->GetName() << "_cxx_h" << std::endl;
  
  // Be sure to include needed headers.
  this->GenerateIncludes(wrapperStream, instantiationStream,
                         package->GetHeaders());
  
  // Begin the recursive generation at the package's starting namespace.
  this->GenerateStartingNamespace(wrapperStream, instantiationStream,
                                  package->GetStartingNamespace());

  // Write the standard #endif at the end of the header file.
  wrapperStream << "#endif" << std::endl;
  
  instantiationStream.close();
  wrapperStream.close();
}


/**
 * Generate the needed #include statements for the wrappers.
 */
void
CxxGenerator
::GenerateIncludes(std::ostream& wrapperStream,
                   std::ostream& instantiationStream,
                   const Headers* headers)
{
  // Make sure we have headers to write out.
  if(!headers)
    {
    return;
    }
  
  // Include every header specified for each purpose.
  for(Headers::FilesIterator header = headers->BeginFiles();
      header != headers->EndFiles(); ++header)
    {
    if(header->purpose == "" || header->purpose == "all")
      {
      wrapperStream << "#include \"" << header->name.c_str() << "\""
                    << std::endl;
      instantiationStream << "#include \"" << header->name.c_str() << "\""
                          << std::endl;
      }
    else if(header->purpose == "instantiate")
      {
      instantiationStream << "#include \"" << header->name.c_str() << "\""
                          << std::endl;
      }
    }
}


/**
 * Entry point for package's starting namespace for generation.
 * Opens all namespaces needed to get from the global namespace to the
 * package's starting namespace, generates the package's namespace, and
 * then closes all namespaces.
 */
void
CxxGenerator
::GenerateStartingNamespace(std::ostream& wrapperStream,
                            std::ostream& instantiationStream,
                            const PackageNamespace* ns)
{
  // Build a stack of enclosing namespaces.
  std::stack<Namespace*>  enclosingNamespaceStack;
  for(Namespace* enclosingNamespace = ns->GetEnclosingNamespace();
      enclosingNamespace && !enclosingNamespace->IsGlobalNamespace();
      enclosingNamespace = enclosingNamespace->GetEnclosingNamespace())
    {
    enclosingNamespaceStack.push(enclosingNamespace);
    }
  
  Indent indent(0);

  // Open all enclosing namespaces.
  while(!enclosingNamespaceStack.empty())
    {
    indent = this->OpenNamespace(wrapperStream, instantiationStream, indent,
                                 enclosingNamespaceStack.top());
    enclosingNamespaceStack.pop();
    }
  
  // Generate the package's namespace.
  this->GenerateNamespace(wrapperStream, instantiationStream, indent, ns);
  
  // Close all enclosing namespaces.
  for(Namespace* enclosingNamespace = ns->GetEnclosingNamespace();
      enclosingNamespace && !enclosingNamespace->IsGlobalNamespace();
      enclosingNamespace = enclosingNamespace->GetEnclosingNamespace())
    {
    indent = this->CloseNamespace(wrapperStream, instantiationStream, indent,
                                  enclosingNamespaceStack.top());
    }
}


/**
 * Generate the C++ wrappers for this namespace and all namespaces
 * nested inside it.
 */
void
CxxGenerator
::GenerateNamespace(std::ostream& wrapperStream,
                    std::ostream& instantiationStream,
                    Indent indent,
                    const PackageNamespace* ns)
{
  // If the namespace has nothing to wrap in it, don't print anything.
  if(ns->GetWrappers().empty())
    {
    return;
    }
  
  indent =
    this->OpenNamespace(wrapperStream, instantiationStream, indent, ns);

  for(PackageNamespace::WrapperIterator wIter = ns->BeginWrappers();
      wIter != ns->EndWrappers(); ++wIter)
    {
    const Named* wrapper = *wIter;
    if(wrapper->IsPackageNamespace())
      {
      this->GenerateNamespace(wrapperStream,
                              instantiationStream,
                              indent,
                              dynamic_cast<const PackageNamespace*>(wrapper));
      }
    else if(wrapper->IsWrapperSet())
      {
      this->GenerateWrapperSet(wrapperStream, indent,
                               dynamic_cast<const WrapperSet*>(wrapper));
      }
    else if(wrapper->IsInstantiationSet())
      {
      this->GenerateInstantiationSet(instantiationStream, indent,
                                     dynamic_cast<const InstantiationSet*>(wrapper));
      }
    }

  this->CloseNamespace(wrapperStream, instantiationStream, indent, ns);
}


/**
 * Generate the code needed for given set of C++ wrappers.
 */
void
CxxGenerator
::GenerateWrapperSet(std::ostream& wrapperStream, const Indent& indent,
                     const WrapperSet* wrapperSet)
{
  for(WrapperSet::ConstIterator wrapper = wrapperSet->Begin();
      wrapper != wrapperSet->End(); ++wrapper)
    {
    // Only display the wrapper's typedef if the names are different.
    if((wrapper->first != "") && (wrapper->first != wrapper->second))
      {
      wrapperStream << indent << "typedef " << wrapper->second << " "
                    << wrapper->first << ";" << std::endl;
      }
    }
}


/**
 * Generate the code needed for given set of C++ template instantiations.
 */
void
CxxGenerator
::GenerateInstantiationSet(std::ostream& instantiationStream,
                           const Indent& indent,
                           const InstantiationSet* instantiationSet)
{
  for(InstantiationSet::ConstIterator wrapper = instantiationSet->Begin();
      wrapper != instantiationSet->End(); ++wrapper)
    {
    if(wrapper->first != wrapper->second)
      {
      instantiationStream << indent << "template " << wrapper->second << ";"
                          << std::endl;
      }
    }
}


/**
 * Print the namespace opening code for the given namespace.
 * Returns the new indentation.
 */
Indent
CxxGenerator
::OpenNamespace(std::ostream& wrapperStream,
                std::ostream& instantiationStream,
                Indent indent, const Namespace* ns) const
{
  if(!ns->IsGlobalNamespace())
    {
    wrapperStream
      << indent << "namespace " << ns->GetName() << std::endl
      << indent << "{" << std::endl;
    instantiationStream
      << indent << "namespace " << ns->GetName() << std::endl
      << indent << "{" << std::endl;
    indent = indent.Next();
    }
  return indent;
}


/**
 * Print the namespace closing code for the given namespace.
 * Returns the new indentation.
 */
Indent
CxxGenerator
::CloseNamespace(std::ostream& wrapperStream,
                 std::ostream& instantiationStream,
                 Indent indent, const Namespace* ns) const
{
  if(!ns->IsGlobalNamespace())
    {
    indent = indent.Previous();
    wrapperStream
      << indent << "} // namespace " << ns->GetName() << std::endl;
    instantiationStream
      << indent << "} // namespace " << ns->GetName() << std::endl;
    }
  return indent;
}


} // namespace gen
