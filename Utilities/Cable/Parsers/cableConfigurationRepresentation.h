/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cableConfigurationRepresentation.h
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
#ifndef _cableConfigurationRepresentation_h
#define _cableConfigurationRepresentation_h

#include "cableReferenceCount.h"

#include <string>
#include <set>
#include <map>
#include <vector>
#include <list>

namespace configuration
{


/**
 * Enumeration of identifiers for object types.
 */
enum TypeOfObject {
  Undefined_id=0,

  CodeBlock_id, Named_id, Class_id, Namespace_id, CableConfiguration_id
};


/**
 * Top-level base class for all configuration objects.
 */
class PARSERS_EXPORT ConfigureObject: public Object
{
public:
  typedef ConfigureObject           Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  virtual const char* GetNameOfClass() const { return "ConfigureObject"; }
  virtual TypeOfObject GetTypeOfObject() const;
  
  bool IsCableConfiguration() const { return (this->GetTypeOfObject() == CableConfiguration_id); }
  bool IsCodeBlock() const          { return (this->GetTypeOfObject() == CodeBlock_id); }
  bool IsClass() const              { return (this->GetTypeOfObject() == Class_id); }
  bool IsNamespace() const          { return (this->GetTypeOfObject() == Namespace_id); }

  virtual void AddCharacterData(const char*, unsigned long, bool);
  
protected:
  ConfigureObject() {}
  ConfigureObject(const Self&) {}
  void operator=(const Self&) {}
  virtual ~ConfigureObject() {}
};


/**
 * Interface to any object that can be referenced with a name in the source.
 */
class PARSERS_EXPORT Named: public ConfigureObject
{
public:
  typedef Named Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  virtual const char* GetNameOfClass() const { return "Named"; }
  virtual TypeOfObject GetTypeOfObject() const { return Named_id; }
  
  const String& GetName() const { return m_Name; }
  
  static Pointer New(const String&);
  
protected:
  Named(const String& in_name): m_Name(in_name) {}
  Named(const Self&) {}
  void operator=(const Self&) {}
  virtual ~Named() {}
private:
  /**
   * The unqualified name by which this is referenced.
   */
  String m_Name;
};


/**
 * Store source code text for custom functions (like create and delete).
 */
class PARSERS_EXPORT CodeBlock: public Named
{
public:
  typedef CodeBlock           Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual const char* GetNameOfClass() const { return "CodeBlock"; }
  virtual TypeOfObject GetTypeOfObject() const { return CodeBlock_id; }
  
  static Pointer New(const String&);
  
  void AddCharacterData(const char*, unsigned long, bool);
  const String& GetCode() const;
  
protected:
  CodeBlock(const String& in_name): Named(in_name) {}
  CodeBlock(const Self&): Named("") {}
  void operator=(const Self&) {}
  virtual ~CodeBlock() {}
  
private:
  /**
   * Hold the code belonging to this CodeBlock.
   */
  String m_Code;
};


/**
 * Represent a class wrapper in the configuration file.  This corresponds to
 * a class in the C++ code being wrapped.
 */
class PARSERS_EXPORT Class: public Named
{
public:
  typedef Class                     Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual const char* GetNameOfClass() const { return "Class"; }
  virtual TypeOfObject GetTypeOfObject() const { return Class_id; }
  
  static Pointer New(const String&);

  typedef std::set<String> AlternateNames;
  AlternateNames::const_iterator AlternateNamesBegin() const { return m_AlternateNames.begin(); }
  AlternateNames::const_iterator AlternateNamesEnd() const { return m_AlternateNames.end(); }
  
  void AddAlternateName(const String&);
  
protected:
  Class(const String& name): Named(name) {}
  Class(const Self&): Named("") {}
  void operator=(const Self&) {}
  virtual ~Class() {}
  
private:
  AlternateNames m_AlternateNames;
};


/**
 * Represent a namespace in the configuration file.  This corresponds to
 * a namespace in the C++ code being wrapped.  It will also serve to hold
 * the names of CodeBlock definitions.
 */
class PARSERS_EXPORT Namespace: public Named
{
public:
  typedef Namespace                 Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;

  virtual const char* GetNameOfClass() const { return "Namespace"; }
  virtual TypeOfObject GetTypeOfObject() const { return Namespace_id; }
  
  static Pointer New(const String&, Namespace*);
  
  String GetQualifiedName() const;
  
  bool AddField(Named*);
  
  bool AddCode(CodeBlock*);
  bool AddNamespace(Namespace*);
  void AddClass(Class*);
  
  typedef std::vector<Class::Pointer> Wrappers;
  Wrappers::const_iterator WrappersBegin() const { return m_Wrappers.begin(); }
  Wrappers::const_iterator WrappersEnd() const { return m_Wrappers.end(); }
  
  typedef std::map<String, Named::Pointer>  Fields;
  Fields::const_iterator FieldsBegin() const { return m_Fields.begin(); }
  Fields::const_iterator FieldsEnd() const { return m_Fields.end(); }  
  
  Named* LookupName(const String&) const;
  
  CodeBlock* LookupCode(const String&) const;
  Namespace* LookupNamespace(const String&) const;
  
  bool IsGlobalNamespace() const { return (m_EnclosingNamespace == NULL); }
  Namespace* GetEnclosingNamespace() const { return m_EnclosingNamespace; }
  
  String GetQualifierString(const String&) const;

protected:
  Namespace(const String&, Namespace*);
  Namespace(const Self&): Named("") {}
  void operator=(const Self&) {}
  virtual ~Namespace() {}
  
private:
  typedef std::vector<String>  Qualifiers;
  typedef Qualifiers::const_iterator QualifiersConstIterator;
  typedef std::back_insert_iterator<Qualifiers>  QualifiersInserter;

  Named* LookupName(QualifiersConstIterator,
                    QualifiersConstIterator,
                    bool walkUpEnclosingScopes) const;
  
  bool ParseQualifiedName(const String&, QualifiersInserter) const;
  
  QualifiersConstIterator Next(QualifiersConstIterator) const;
protected:  
  /**
   * The enclosing namespace.  NULL only for the global namespace.
   * This is a real pointer to avoid circular references.
   */
  Namespace* m_EnclosingNamespace;

  /**
   * The set of fields that have been defined in this Namespace.
   */
  Fields m_Fields;

  Wrappers m_Wrappers;
};


/**
 * A collection of all configuration information from an input file.
 */
class PARSERS_EXPORT CableConfiguration: public ConfigureObject
{
public:
  typedef CableConfiguration        Self;
  typedef SmartPointer<Self>        Pointer;
  typedef SmartPointer<const Self>  ConstPointer;
  
  virtual const char* GetNameOfClass() const { return "CableConfiguration"; }
  virtual TypeOfObject GetTypeOfObject() const { return CableConfiguration_id; }

  static Pointer New(const String&, const String&, const String&);
  
  Namespace::Pointer GetGlobalNamespace() const
    { return m_GlobalNamespace; }
  
  const String& GetSourceFileName() const
    { return m_SourceFileName; }
  const String& GetGroupName() const
    { return m_GroupName; }
  const String& GetPackageName() const
    { return m_PackageName; }
  
  typedef std::vector<String> Headers;
  Headers::const_iterator HeadersBegin() const { return m_Headers.begin(); }
  Headers::const_iterator HeadersEnd() const { return m_Headers.end(); }
  void AddHeader(const String& s) { m_Headers.push_back(s); }

  typedef std::set<String> Groups;
  Groups::const_iterator GroupsBegin() const { return m_Groups.begin(); }
  Groups::const_iterator GroupsEnd() const { return m_Groups.end(); }
  void AddGroup(const String& s) { m_Groups.insert(s); }

  
protected:
  CableConfiguration(const String&, const String&, const String&);
  CableConfiguration(const Self&) {}
  void operator=(const Self&) {}
  virtual ~CableConfiguration() {}
  
private:
  /**
   * The global namespace defined in the configuration.  All other namespaces
   * are contained within it.
   */
  Namespace::Pointer m_GlobalNamespace;
  
  Headers m_Headers;

  Groups m_Groups;
  
  String m_SourceFileName;
  
  String m_GroupName;
  
  String m_PackageName;
};


} // namespace configuration

#endif
