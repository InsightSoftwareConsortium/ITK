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
