#include "configRep.h"

namespace configuration
{


/**
 * Get type identifier of object.  Default to Undefined_id.
 */
TypeOfObject
ConfigureObject
::GetTypeOfObject(void) const
{
  return Undefined_id;
}


/**
 * Default character data handler.  This just ignores the data.  Subclasses
 * of ConfigureObject can override this method to do something with the
 * data.
 *
 * The first argument is a pointer to the data (not null-terminated).
 * The second argument is the length of the data's block of memory.
 * The third argument is whether the data is inside a <![CDATA[...]]> block.
 */
void
ConfigureObject
::AddCharacterData(const char*, unsigned long, bool)
{
}


/**
 * Create a new Dependencies and return a pointer to it.
 */
Dependencies::Pointer
Dependencies
::New()
{
  return new Dependencies;
}


/**
 * Add another package name to the list of dependencies.
 */
void
Dependencies
::Add(const String& dep)
{
  m_PackageNames.push_back(dep);
}


/**
 * Create a new CodeBlock and return a pointer to it.
 */
CodeBlock::Pointer
CodeBlock
::New()
{
  return new CodeBlock;
}


/**
 * Constructor initializes CodeBlock's string to be empty.
 */
CodeBlock
::CodeBlock():
  m_Code("")
{
}


/**
 * Character data in a CodeBlock are treated as lines of code.
 * Add this line to the CodeBlock.
 */
void
CodeBlock
::AddCharacterData(const char* line, unsigned long len, bool)
{
  m_Code.append(line, len);
}


/**
 * Print the CodeBlock's lines to the output stream.
 */
void
CodeBlock
::PrintCode(std::ostream& os) const
{
  os << m_Code.c_str();
}


/**
 * Get this CodeBlock's code as a String.
 */
const String&
CodeBlock
::GetCode() const
{
  return m_Code;
}


/**
 * Create a new Argument and return a pointer to it.
 */
Argument::Pointer
Argument
::New(const String& tag)
{
  return new Argument(tag);
}


/**
 * Constructor sets up the tag of the Argument.
 */
Argument
::Argument(const String& tag):
  m_Tag(tag)
{
}


/**
 * Get the tag associated with this argument.
 */
const String&
Argument
::GetTag() const
{
  return m_Tag;
}


/**
 * Create a new ArgumentSet and return a pointer to it.
 */
ArgumentSet::Pointer
ArgumentSet
::New()
{
  return new ArgumentSet;
}


/**
 * Add a new argument to the set. 
 */
void
ArgumentSet
::Add(const String& tag, const String& code)
{
  m_Arguments[tag] = code;
}


/**
 * Add all the arguments in the given ArgumentSet to this one.
 */
void
ArgumentSet
::Add(const ArgumentSet* argumentSet)
{
  m_Arguments.insert(argumentSet->Begin(), argumentSet->End());
}


/**
 * Print the argument set to the given stream.
 */
void
ArgumentSet
::Print(std::ostream& os) const
{
  for(ConstIterator a = this->Begin(); a != this->End(); ++a)
    {
    os << a->first.c_str() << ": " << a->second.c_str() << std::endl;
    }
}


/**
 * Create a new Headers and return a pointer to it.
 */
Headers::Pointer
Headers
::New()
{
  return new Headers;
}


/**
 * Add a file to the set of header files.
 */
void
Headers
::AddFile(const String& name)
{
  m_Files.insert(name);
}


/**
 * Add a directory to the set of header directories.
 */
void
Headers
::AddDirectory(const String& name)
{
  m_Directories.insert(name);
}


/**
 * Create a new Package and return a pointer to it.
 */
Package::Pointer
Package
::New(const String& name)
{
  return new Package(name);
}


/**
 * Constructor sets up the name of the package.
 */
Package
::Package(const String& name):
  m_Name(name)
{
}


/**
 * Get the name of this package.
 */
const String&
Package
::GetName() const
{
  return m_Name;
}


} // namespace configuration
