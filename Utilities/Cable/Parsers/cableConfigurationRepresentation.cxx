/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cableConfigurationRepresentation.cxx
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
#include "cableConfigurationRepresentation.h"

#include <iostream>

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
 * Create a new Named and return a pointer to it.
 */
Named::Pointer
Named
::New(const String& in_name)
{
  return new Named(in_name);
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
 * Get this CodeBlock's code as a String.
 */
const String&
CodeBlock
::GetCode() const
{
  return m_Code;
}

/**
 * Create a new Class and return a pointer to it.
 */
Class::Pointer
Class
::New(const String& in_name)
{
  return new Class(in_name);
}  


/**
 * Add an alternate name for the class.
 */
void
Class
::AddAlternateName(const String& in_name)
{
  m_AlternateNames.insert(in_name);
}


/**
 * Create a new Namespace and return a pointer to it.
 */
Namespace::Pointer
Namespace
::New(const String& in_name, Namespace* in_enclosingNamespace)
{
  return new Namespace(in_name, in_enclosingNamespace);
}  
  

/**
 * Return the fully qualified name of this namespace.
 */
String
Namespace
::GetQualifiedName() const
{
  String name = "";
  if(m_EnclosingNamespace)
    {
    name = m_EnclosingNamespace->GetQualifiedName() + "::";
    }
  name += this->GetName();
  return name;
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
    // The name is already used for another field.
    return false;
    }
  else
    {
    // Add the field.
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
 * Add the given class to the set of wrappers defined in this namespace.
 */
void
Namespace
::AddClass(Class* c)
{
  m_Wrappers.push_back(c);
}


/**
 * Lookup the given name starting in the scope of this namespace.
 */
Named*
Namespace
::LookupName(const String& name) const
{
  // Parse the name into its qualifiers.
  Qualifiers qualifierList;
  
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
 * Lookup the given CodeBlock starting in the scope of this namespace.
 */
CodeBlock*
Namespace
::LookupCode(const String& name) const
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
::LookupNamespace(const String& name) const
{
  // Lookup the name.
  Named* field = this->LookupName(name);
  
  // Make sure it is a Namespace.
  if(!field || !(field->IsNamespace()))
    {
    return NULL;
    }
  
  // Return the Namespace found.
  return dynamic_cast<Namespace*>(field);
}


/**
 * Given a (possibly) qualified name, return a string with all the
 * qualifiers, ending in a "::".
 */
String
Namespace
::GetQualifierString(const String& name) const
{
  Qualifiers qualifierList;
  if(this->ParseQualifiedName(name, std::back_inserter(qualifierList)))
    {
    // The name was valid.  Use all but the last qualifier.
    if(qualifierList.size() > 1)
      {
      qualifierList.pop_back();
      String qualifiers = "";
      for(Qualifiers::const_iterator q = qualifierList.begin();
          q != qualifierList.end(); ++q)
        {
        qualifiers.append(*q);
        qualifiers.append("::");
        }
      return qualifiers;
      }
    else
      {
      return "";
      }
    }
  else
    {
    // The name was invalid, and failed to parse.
    return "";
    }
}


/**
 * Constructor initializes the name and enclosing namespace.
 */
Namespace
::Namespace(const String& in_name, Namespace* in_enclosingNamespace):
  Named(in_name),
  m_EnclosingNamespace(in_enclosingNamespace)
{
}


/**
 * Lookup the given name starting in the scope of this namespace.
 *
 * This internal version takes iterators into a Qualifiers describing
 * the name.
 */
Named*
Namespace
::LookupName(QualifiersConstIterator first,
             QualifiersConstIterator last,
             bool walkUpEnclosingScopes) const
{
  // If there is no name, we cannot look it up.
  if(first == last)
    {
    return NULL;
    }
  
  // Get an iterator to the second member of the list (may be the end).
  QualifiersConstIterator second = first; ++second;
  
  // Try to look up the highest level qualifier in this namespace.
  Fields::const_iterator fieldIter = m_Fields.find(*first);
  if(fieldIter != m_Fields.end())
    {
    // A field was found.  See what type it is.
    Named* field = fieldIter->second;
    if(!(field->IsNamespace()))
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
 * qualifier out through the QualifiersInserter.  The given name
 * may not end in a ::, but if it begins in a ::, then the first qualifier
 * will be the empty string.
 *
 * Returns false only if there was an error parsing the name.
 */
bool
Namespace
::ParseQualifiedName(const String& name, QualifiersInserter qualifiers) const
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
 * Given a QualifiersConstIterator "iter", return the equivalent iterator
 * to "++iter".
 */
Namespace::QualifiersConstIterator
Namespace
::Next(QualifiersConstIterator iter) const
{
  return ++iter;
}


/**
 * Create a new CableConfiguration and return a pointer to it.
 */
CableConfiguration::Pointer
CableConfiguration
::New(const String& source, const String& group, const String& package)
{
  return new CableConfiguration(source, group, package);
}


/**
 * Constructor allocates the global Namespace (which has no name or
 * enclosing namespace).
 */
CableConfiguration
::CableConfiguration(const String& source, const String& group,
                     const String& package):
  m_GlobalNamespace(Namespace::New("", NULL)),
  m_SourceFileName(source),
  m_GroupName(group),
  m_PackageName(package)
{
}

} // namespace configuration
