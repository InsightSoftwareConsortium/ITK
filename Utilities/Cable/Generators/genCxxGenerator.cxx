#include "genCxxGenerator.h"

#include <fstream>

namespace gen
{

using namespace configuration;

/**
 * Print indentation spaces.
 */
void
Indent
::Print(std::ostream& os) const
{
  // Use blocks of 8 spaces to speed up big indents.
  unsigned int blockCount = m_Indent >> 3;
  unsigned int singleCount = m_Indent & 7;
  while(blockCount-- > 0)
    {
    os << "        ";
    }
  while(singleCount-- > 0)
    {
    os << " ";
    }
}


/**
 * Simplify indentation printing by allowing Indent objects to be added
 * to streams.
 */
std::ostream& operator<<(std::ostream& os, const Indent& indent)
{  
  indent.Print(os);
  return os;
}


/**
 * Simplify the printing of strings.
 */
std::ostream& operator<<(std::ostream& os, const String& str)
{
  os << str.c_str();
  return os;
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
  String wrapperFile = "Cxx/"+package->GetName()+"_cxx.h";
  String instantiationFile = "Cxx/"+package->GetName()+"_cxx.cxx";
  
  // Open the output files.
  ofstream wrapperStream(wrapperFile.c_str());
  if(!wrapperStream) { return; }
  ofstream instantiationStream(instantiationFile.c_str());
  if(!instantiationStream) { return; }
  
  // Write the standard #ifndef/#define pair in the header file.
  wrapperStream << "#ifndef _" << package->GetName() << "_cxx_h" << std::endl;
  wrapperStream << "#define _" << package->GetName() << "_cxx_h" << std::endl;
  
  // Be sure to include needed headers.
  this->GenerateIncludes(wrapperStream, instantiationStream,
                         package->GetHeaders());
  
  // Begin the recursive generation at the global namespace.
  this->GenerateNamespace(wrapperStream, instantiationStream, Indent(-2),
                          package->GetGlobalNamespace());

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
 * Generate the C++ wrappers for this namespace and all namespaces
 * nested inside it.
 */
void
CxxGenerator
::GenerateNamespace(std::ostream& wrapperStream,
                    std::ostream& instantiationStream,
                    const Indent& indent,
                    const PackageNamespace* ns)
{
  // If the namespace has nothing to wrap in it, don't print anything.
  if(ns->GetWrappers().empty())
    {
    return;
    }
  
  // Only print namespace begin code if not global namespace.
  if(!ns->IsGlobalNamespace())
    {
    wrapperStream << indent << "namespace " << ns->GetName() << std::endl
                  << indent << "{" << std::endl;
    instantiationStream << indent << "namespace " << ns->GetName() << std::endl
                        << indent << "{" << std::endl;
    }

  for(PackageNamespace::WrapperIterator wIter = ns->BeginWrappers();
      wIter != ns->EndWrappers(); ++wIter)
    {
    const Named* wrapper = *wIter;
    if(wrapper->IsPackageNamespace())
      {
      this->GenerateNamespace(wrapperStream,
                              instantiationStream,
                              indent.Next(),
                              dynamic_cast<const PackageNamespace*>(wrapper));
      }
    else if(wrapper->IsWrapperSet())
      {
      this->GenerateWrapperSet(wrapperStream, indent.Next(),
                               dynamic_cast<const WrapperSet*>(wrapper));
      }
    else if(wrapper->IsInstantiationSet())
      {
      this->GenerateInstantiationSet(instantiationStream, indent.Next(),
                                     dynamic_cast<const InstantiationSet*>(wrapper));
      }
    }
  
  // Only print namespace end code if not global namespace.  
  if(!ns->IsGlobalNamespace())
    {
    wrapperStream << indent << "} // namespace " << ns->GetName()
                  << std::endl;
    instantiationStream << indent << "} // namespace " << ns->GetName()
                        << std::endl;
    }
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
    if(wrapper->first != wrapper->second)
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


} // namespace gen
