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
 * Character data in an Element are treated as lines of code.
 * Add this line to the Element's code.
 */
void
Element
::AddCharacterData(const char* line, unsigned long len, bool)
{
  m_Code.append(line, len);
}


/**
 * Create a new Element and return a pointer to it.
 */
Element::Pointer
Element
::New(const String& tag)
{
  return new Element(tag);
}


/**
 * Constructor sets up the tag of the Element.
 */
Element
::Element(const String& tag):
  m_Tag(tag)
{
}


/**
 * Get the tag associated with this element.
 */
const String&
Element
::GetTag() const
{
  return m_Tag;
}


/**
 * Get this Element's code as a String.
 */
const String&
Element
::GetCode() const
{
  return m_Code;
}


/**
 * Create a new CodeBlock and return a pointer to it.
 */
CodeBlock::Pointer
CodeBlock
::New(const String& in_name)
{
  return new CodeBlock(in_name);
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
 * Create a new Set and return a pointer to it.
 */
Set::Pointer
Set
::New(const String& in_name)
{
  return new Set(in_name);
}


/**
 * Add a new element to the set. 
 */
void
Set
::Add(const String& tag, const String& code)
{
  m_Elements[tag] = code;
}


/**
 * Add all the elements in the given Set to this one.
 */
void
Set
::Add(const Set* elementSet)
{
  m_Elements.insert(elementSet->Begin(), elementSet->End());
}


/**
 * Print the element set to the given stream.
 */
void
Set
::Print(std::ostream& os) const
{
  for(ConstIterator a = this->Begin(); a != this->End(); ++a)
    {
    os << a->first.c_str() << ": " << a->second.c_str() << std::endl;
    }
}


/**
 * Create a new WrapperSet and return a pointer to it.
 */
WrapperSet::Pointer
WrapperSet
::New()
{
  return new WrapperSet;
}


/**
 * Create a new Namespace and return a pointer to it.
 */
Namespace::Pointer
Namespace
::New(const String& in_name, const String& in_prefixSeparator,
      Namespace* in_enclosingNamespace)
{
  return new Namespace(in_name, in_prefixSeparator, in_enclosingNamespace);
}
  

/**
 * Add a new Named entity to this Namespace's fields.
 * Returns false only if the field's name already exists.
 */
bool
Namespace
::AddField(Named* field)
{
  if(m_Fields.count(field->GetName()) > 0)
    {
    return false;
    }
  else
    {
    m_Fields[field->GetName()] = field;
    return true;
    }
}


/**
 * Add a CodeBlock to this Namespace.
 * Returns false only if the CodeBlock's name already exists.
 */
bool
Namespace
::AddCode(CodeBlock* cb)
{
  return this->AddField(cb);
}

/**
 * Add a Set to this Namespace.
 * Returns false only if the Set's name already exists.
 */
bool
Namespace
::AddSet(Set* set)
{
  return this->AddField(set);
}


/**
 * Add a nested Namespace to this Namespace.
 * Returns false only if the Namespace's name already exists.
 */
bool
Namespace
::AddNamespace(Namespace* ns)
{
  return this->AddField(ns);
}


/**
 * Lookup the given name starting in the scope of this namespace.
 */
Named*
Namespace
::LookupName(const String& name)
{
  // Parse the name into its qualifiers.
  QualifierList qualifierList;
  
  if(this->ParseQualifiedName(name, std::back_inserter(qualifierList)))
    {
    // The name was valid, but may or may not exist.  Try to look it up.
    return this->LookupName(qualifierList.begin(), qualifierList.end(), true);
    }
  else
    {
    // The name was invalid, and failed to parse.
    return NULL;
    }
}


/**
 * Lookup the given Set starting in the scope of this namespace.
 */
Set*
Namespace
::LookupSet(const String& name)
{
  // Lookup the name.
  Named* field = this->LookupName(name);
  
  // Make sure it is a Set.
  if(!field || !field->IsSet())
    {
    return NULL;
    }
  
  // Return the Set found.
  return dynamic_cast<Set*>(field);
}


/**
 * Lookup the given CodeBlock starting in the scope of this namespace.
 */
CodeBlock*
Namespace
::LookupCode(const String& name)
{
  // Lookup the name.
  Named* field = this->LookupName(name);
  
  // Make sure it is a CodeBlock.
  if(!field || !field->IsCodeBlock())
    {
    return NULL;
    }
  
  // Return the CodeBlock found.
  return dynamic_cast<CodeBlock*>(field);
}


/**
 * Lookup the given Namespace starting in the scope of this namespace.
 */
Namespace*
Namespace
::LookupNamespace(const String& name)
{
  // Lookup the name.
  Named* field = this->LookupName(name);
  
  // Make sure it is a Namespace.
  if(!field || !field->IsNamespace())
    {
    return NULL;
    }
  
  // Return the Namespace found.
  return dynamic_cast<Namespace*>(field);
}


/**
 * Constructor initializes the name, prefix separator, and enclosing
 * namespace.
 */
Namespace
::Namespace(const String& in_name, const String& in_prefixSeparator,
            Namespace* in_enclosingNamespace):
  Named(in_name),
  m_PrefixSeparator(in_prefixSeparator),
  m_EnclosingNamespace(in_enclosingNamespace)
{
}


/**
 * Lookup the given name starting in the scope of this namespace.
 *
 * This internal version takes iterators into a QualifierList describing
 * the name.
 */
Named*
Namespace
::LookupName(QualifierListConstIterator first,
             QualifierListConstIterator last,
             bool walkUpEnclosingScopes)
{
  // If there is no name, we cannot look it up.
  if(first == last)
    {
    return NULL;
    }
  
  // Get an iterator to the second member of the list (may be the end).
  QualifierListConstIterator second = first; ++second;
  
  // Try to look up the highest level qualifier in this namespace.
  Fields::const_iterator fieldIter = m_Fields.find(*first);
  if(fieldIter != m_Fields.end())
    {
    // A field was found.  See what type it is.
    Named* field = fieldIter->second;
    if(!field->IsNamespace())
      {
      // The field is not a namespace.  Make sure it is the last qualifier.
      if(second == last)
        {
        // We have found the name.
        return field;
        }
      else
        {
        // A non-namespace name has been used as a qualifier.
        return NULL;
        }
      }
    else
      {
      // We have found a nested namespace.
      Namespace* ns = dynamic_cast<Namespace*>(field);
      if(second == last)
        {
        // This was the last qualifier.  This is the target.
        return ns;
        }
      else
        {
        // Lookup the rest of the name in the nested namespace.
        return ns->LookupName(second, last, false);
        }
      }
    }
  else
    {
    // Couldn't find the qualifier here.
    // Move up one namespace scope, and try again.
    if(m_EnclosingNamespace)
      {
      // Make sure we are allowed to look up to enclosing scopes.
      if(walkUpEnclosingScopes)
        {
        return m_EnclosingNamespace->LookupName(first, last, true);
        }
      else
        {
        return NULL;
        }
      }
    else
      {
      // We are at the global namespace.  The qualifier is only valid
      // if it is the empty string.
      if(first->length() == 0)
        {
        // The explicit "::" global namespace qualifier preceded the name.
        // Look it up in this global namespace without the qualifier.
        return this->LookupName(second, last, false);
        }
      else
        {
        // Couldn't find the name.
        return NULL;
        }
      }
    }
}


/**
 * Parse a ::-separated qualified name into its components.  Write each
 * qualifier out through the QualifierListInserter.  The given name
 * may not end in a ::, but if it begins in a ::, then the first qualifier
 * will be the empty string.
 *
 * Returns false only if there was an error parsing the name.
 */
bool
Namespace
::ParseQualifiedName(const String& name, QualifierListInserter qualifiers) const
{  
  String qualifier = "";
  for(String::const_iterator c = name.begin(); c != name.end(); ++c)
    {
    if(*c != ':')
      {
      // This is just another character.  Add it to the qualifier.
      qualifier.insert(qualifier.end(), *c);
      }
    else
      {
      // This is the first ':' of a '::' seperator.
      // Output current qualifier.  This may be empty only if the string
      // begins with a :: seperator.
      *qualifiers++ = qualifier;
      
      // Reset for next qualifier.
      qualifier = "";
      
      // Move past the '::' seperator (it shouldn't be the last character).
      if((++c != name.end()) && (*c == ':')
         && (++c != name.end()) && (*c != ':'))
        {
        // Handle this first character now.  The loop will handle the rest.
        qualifier.insert(qualifier.end(), *c);        
        }
      else
        {
        // Invalid qualified identifier.
        return false;
        }
      }
    }
  
  // Output the last qualifier.  This should never be empty unless the
  // string was empty.
  if(qualifier.length() > 0)
    *qualifiers++ = qualifier;
  
  return true;
}


/**
 * Given a QualifierListConstIterator "iter", return the equivalent iterator
 * to "++iter".
 */
Namespace::QualifierListConstIterator
Namespace
::Next(QualifierListConstIterator iter) const
{
  return ++iter;
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
 * Constructor sets up the name of the package and allocates the
 * global Namespace (which has no name, prefix separator, or enclosing
 * namespace).
 */
Package
::Package(const String& name):
  m_Name(name),
  m_GlobalNamespace(Namespace::New("", "", NULL))
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
