#include "genCxxGenerator.h"

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
 * Generate the C++ wrappers to the given output stream.
 */
void
CxxGenerator
::Generate(std::ostream& os)
{
  Namespace::ConstPointer globalNamespace =
    m_Package->GetGlobalNamespace().RealPointer();
  
  // We want everything in the global namespace to start in the first column.
  Indent indent(-2);
  
  // Start the output with the global namespace of definitions.
  this->GenerateNamespace(os, indent, globalNamespace);
}
 
  
/**
 * Generate the C++ wrappers for this namespace and all namespaces
 * nested inside it.
 */
void
CxxGenerator
::GenerateNamespace(std::ostream& os, const Indent& indent,
                    const Namespace* ns)
{
  if(!ns->IsGlobalNamespace())
    {
    os << indent << "namespace " << ns->GetName() << std::endl
       << indent << "{" << std::endl;
    }

  for(Namespace::WrapperIterator wIter = ns->BeginWrapperList();
      wIter != ns->EndWrapperList(); ++wIter)
    {
    const Named* wrapper = *wIter;
    if(wrapper->IsNamespace())
      {
      this->GenerateNamespace(os, indent.Next(),
                              dynamic_cast<const Namespace*>(wrapper));
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
