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
  String fileName = "Cxx/"+package->GetName()+"_cxx.cxx";
  
  // Open the output file.
  ofstream out(fileName.c_str());
  if(!out)
    {
    // ERROR!
    return;
    }
  
  // Be sure to include needed headers.
  this->GenerateHeaderIncludes(out, package->GetHeaders());
  
  // Begin the recursive generation at the global namespace.
  this->GenerateNamespace(out, Indent(-2),
                          package->GetGlobalNamespace());
  
  out.close();
}


/**
 * Generate the needed #includes.
 */
void
CxxGenerator
::GenerateHeaderIncludes(std::ostream& os,
                         const Headers* headers)
{
  // Make sure we have headers to write out.
  if(!headers)
    {
    return;
    }
  
  // Include every header specified.
  for(Headers::FilesIterator header = headers->BeginFiles();
      header != headers->EndFiles(); ++header)
    {
    os << "#include \"" << header->c_str() << "\"" << std::endl;
    }
}


/**
 * Generate the C++ wrappers for this namespace and all namespaces
 * nested inside it.
 */
void
CxxGenerator
::GenerateNamespace(std::ostream& os, const Indent& indent,
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
    os << indent << "namespace " << ns->GetName() << std::endl
       << indent << "{" << std::endl;
    }

  for(PackageNamespace::WrapperIterator wIter = ns->BeginWrappers();
      wIter != ns->EndWrappers(); ++wIter)
    {
    const Named* wrapper = *wIter;
    if(wrapper->IsPackageNamespace())
      {
      this->GenerateNamespace(os, indent.Next(),
                              dynamic_cast<const PackageNamespace*>(wrapper));
      }
    else if(wrapper->IsWrapperSet())
      {
      this->GenerateWrapperSet(os, indent.Next(),
                               dynamic_cast<const WrapperSet*>(wrapper));
      }
    else if(wrapper->IsInstantiationSet())
      {
      this->GenerateInstantiationSet(os, indent.Next(),
                                     dynamic_cast<const InstantiationSet*>(wrapper));
      }
    else
      {
      os << "ERROR!!!\n";
      }
    }
  
  // Only print namespace end code if not global namespace.  
  if(!ns->IsGlobalNamespace())
    {
    os << indent << "} // namespace " << ns->GetName() << std::endl;
    }
}


/**
 * Generate the code needed for given set of C++ wrappers.
 */
void
CxxGenerator
::GenerateWrapperSet(std::ostream& os, const Indent& indent,
                     const WrapperSet* wrapperSet)
{
  for(WrapperSet::ConstIterator wrapper = wrapperSet->Begin();
      wrapper != wrapperSet->End(); ++wrapper)
    {
    // Only display the wrapper's typedef if the names are different.
    if(wrapper->first != wrapper->second)
      {
      os << indent << "typedef " << wrapper->second << " "
         << wrapper->first << ";" << std::endl;
      }
    }
}


/**
 * Generate the code needed for given set of C++ template instantiations.
 */
void
CxxGenerator
::GenerateInstantiationSet(std::ostream& os, const Indent& indent,
                     const InstantiationSet* instantiationSet)
{
  for(InstantiationSet::ConstIterator wrapper = instantiationSet->Begin();
      wrapper != instantiationSet->End(); ++wrapper)
    {
    if(wrapper->first != wrapper->second)
      {
      os << indent << "template " << wrapper->second << ";" << std::endl;
      }
    }
}


} // namespace gen
