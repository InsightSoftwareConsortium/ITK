/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cableSourceRepresentation.cxx
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
#include "cableSourceRepresentation.h"

#include <iostream>

#include <stdio.h>
#include <stdlib.h>
#include <strstream>

namespace source
{

/**
 * A singe instance of the TypeSystem will be used to register all
 * cxx type representations.  This is it.
 */
cxx::TypeSystem CxxTypes::typeSystem;

/**
 * Register an ArrayType having the given element type and size.
 */
cxx::CvQualifiedType
CxxTypes::GetArrayType(const cxx::CvQualifiedType& elementType,
                       unsigned long size)
{
  return typeSystem.GetArrayType(elementType, size)
    ->GetCvQualifiedType(false, false);
}


/**
 * Register a ClassType having the given name, cv-qualifiers, and
 * (optionally) parents.
 */
cxx::CvQualifiedType
CxxTypes::GetClassType(const String& name,
                       bool isConst, bool isVolatile,
                       bool isAbstract, const cxx::ClassTypes& parents)
{
  return typeSystem.GetClassType(name, isAbstract, parents)
    ->GetCvQualifiedType(isConst, isVolatile);
}


/**
 * Register an EnumerationType having the given name, and cv-qualifiers.
 */
cxx::CvQualifiedType
CxxTypes::GetEnumerationType(const String& name,
                             bool isConst, bool isVolatile)
{
  return typeSystem.GetEnumerationType(name)
    ->GetCvQualifiedType(isConst, isVolatile);
}


/**
 * Register a FunctionType having the given return type, argument types,
 * and cv-qualifiers.
 */
cxx::CvQualifiedType
CxxTypes::GetFunctionType(const cxx::CvQualifiedType& returnType,
                          const cxx::CvQualifiedTypes& argumentTypes,
                          bool isConst, bool isVolatile)
{
  return typeSystem.GetFunctionType(returnType, argumentTypes)
    ->GetCvQualifiedType(isConst, isVolatile);
}


/**
 * Register a FundamentalType having the given type id and cv-qualifiers.
 */
cxx::CvQualifiedType
CxxTypes::GetFundamentalType(cxx::FundamentalType::Id id,
                             bool isConst, bool isVolatile)
{
//#ifdef _wrap_NO_WCHAR_T
//  if(id == FundamentalType::WChar_t) { id = FundamentalType::UnsignedShortInt; }
//#endif
  return typeSystem.GetFundamentalType(id)
    ->GetCvQualifiedType(isConst, isVolatile);
}


/**
 * Register a PointerType pointing to the given type and having the
 * given cv-qualifiers.
 */
cxx::CvQualifiedType
CxxTypes::GetPointerType(const cxx::CvQualifiedType& referencedType,
                         bool isConst, bool isVolatile)
{
  return typeSystem.GetPointerType(referencedType)
    ->GetCvQualifiedType(isConst, isVolatile);
}


/**
 * Register a PointerToMemberType pointing to the given type inside the
 * given ClassType and having the given cv-qualifiers.
 */
cxx::CvQualifiedType
CxxTypes::GetPointerToMemberType(const cxx::CvQualifiedType& referencedType,
                                 const cxx::ClassType* classScope,
                                 bool isConst, bool isVolatile)
{
  return typeSystem.GetPointerToMemberType(referencedType, classScope)
    ->GetCvQualifiedType(isConst, isVolatile);
}


/**
 * Register a ReferenceType referencing the given type.
 */
cxx::CvQualifiedType
CxxTypes::GetReferenceType(const cxx::CvQualifiedType& referencedType)
{
  return typeSystem.GetReferenceType(referencedType)
    ->GetCvQualifiedType(false, false);
}


/**
 * Convert the given string to a valid C identifier.
 */
String GetValid_C_Identifier(const String& in_name)
{
  static const char* builtin[][2]
    = { {"+", "__plus"},
        {"+=", "__aplus"},
        {"*=", "__amul"},
        {"/=", "__adiv"},
        {"-", "__minus"},
        {"-=", "__aminus"},
        {"=", "__aeq"},
        {"==", "__eq"},
        {"()", "__func"},
        {0, 0} };
  
  for(int i=0; builtin[i][0]; ++i)
    {
    if(in_name == builtin[i][0])
      {
      return builtin[i][1];
      }
    }

  std::strstream name;
  
  for(String::const_iterator ch = in_name.begin(); ch != in_name.end(); ++ch)
    {
    switch(*ch)
      {
      case ' ': name << '_'; break;
      case ',': break;
      case '/': name << 'd'; break;
      case ':': name << 'c'; break;
      case '<': name << "la"; break;
      case '>': name << "ra"; break;
      case '[': name << "ls"; break;
      case ']': name << "rs"; break;
      case '*': name << "p"; break;
      case '&': name << "r"; break;
      default:  name << *ch; break;
      }
    }
  
  name << std::ends;
  
  return name.str();
}

Named::Pointer
Named
::New(const String& name)
{
  return new Named(name);
}

/**
 * Construct a new Location and return a smart pointer to it.
 */
Location::Pointer
Location
::New(const String& file, unsigned int line)
{
  return new Location(file, line);
}


/**
 * Construct a new CvQualifiers and return a smart pointer to it.
 */
CvQualifiers::Pointer
CvQualifiers
::New(bool is_const, bool is_volatile, bool is_restrict)
{
  return new CvQualifiers(is_const, is_volatile, is_restrict);
}


/**
 * Construct a new Argument and return a smart pointer to it.
 */
Argument::Pointer
Argument
::New(const String& name)
{
  return new Argument(name);
}


/**
 * Construct a new Returns and return a smart pointer to it.
 */
Returns::Pointer
Returns
::New()
{
  return new Returns;
}


/**
 * Construct a new NamedType and return a smart pointer to it.
 */
NamedType::Pointer
NamedType
::New()
{
  return new NamedType;
}


/**
 * Construct a new PointerType and return a smart pointer to it.
 */
PointerType::Pointer
PointerType
::New()
{
  return new PointerType;
}



/**
 * Construct a new ReferenceType and return a smart pointer to it.
 */
ReferenceType::Pointer
ReferenceType
::New()
{
  return new ReferenceType;
}


/**
 * Construct a new FunctionType and return a smart pointer to it.
 */
FunctionType::Pointer
FunctionType
::New()
{
  return new FunctionType;
}


/**
 * Construct a new MethodType and return a smart pointer to it.
 */
MethodType::Pointer
MethodType
::New()
{
  return new MethodType;
}


/**
 * Construct a new OffsetType and return a smart pointer to it.
 */
OffsetType::Pointer
OffsetType
::New()
{
  return new OffsetType;
}


/**
 * Construct a new ArrayType and return a smart pointer to it.
 */
ArrayType::Pointer
ArrayType
::New(int min, int max)
{
  return new ArrayType(min, max);
}


/**
 * Construct a new Typedef and return a smart pointer to it.
 */
Typedef::Pointer
Typedef
::New(const String& name)
{
  return new Typedef(name);
}


/**
 * Construct a new Enumeration and return a smart pointer to it.
 */
Enumeration::Pointer
Enumeration
::New(const String& name)
{
  return new Enumeration(name);
}


/**
 * Construct a new Function and return a smart pointer to it.
 */
Function::Pointer
Function
::New(const String& name)
{
  return new Function(name);
}


/**
 * Construct a new Namespace and return a smart pointer to it.
 */
Namespace::Pointer
Namespace
::New(const String& name)
{
  return new Namespace(name);
}


/**
 * Construct a new Method and return a smart pointer to it.
 */
Method::Pointer
Method
::New(const String& name, Access access, bool is_static, bool is_const,
      bool is_virtual, bool is_pure_virtual)
{
  return new Method(name, access, is_static, is_const, is_virtual,
                    is_pure_virtual);
}


/**
 * Construct a new Constructor and return a smart pointer to it.
 */
Constructor::Pointer
Constructor
::New(Access access)
{
  return new Constructor(access);
}


/**
 * Construct a new Destructor and return a smart pointer to it.
 */
Destructor::Pointer
Destructor
::New(Access access, bool is_virtual)
{
  return new Destructor(access, is_virtual);
}


/**
 * Construct a new Converter and return a smart pointer to it.
 */
Converter::Pointer
Converter
::New(Access access, bool is_const, bool is_virtual, bool is_pure_virtual)
{
  return new Converter(access, is_const, is_virtual, is_pure_virtual);
}


/**
 * Construct a new OperatorMethod and return a smart pointer to it.
 */
OperatorMethod::Pointer
OperatorMethod
::New(const String& name, Access access, bool is_static,
      bool is_const, bool is_virtual, bool is_pure_virtual)
{
  return new OperatorMethod(name, access, is_static, is_const,
                            is_virtual, is_pure_virtual);
}


/**
 * Construct a new OperatorFunction and return a smart pointer to it.
 */
OperatorFunction::Pointer
OperatorFunction
::New(const String& name)
{
  return new OperatorFunction(name);
}


/**
 * Construct a new Class and return a smart pointer to it.
 */
Class::Pointer
Class
::New(const String& name, Access access, bool is_abstract)
{
  return new Class(name, access, is_abstract);
}


/**
 * Construct a new Struct and return a smart pointer to it.
 */
Struct::Pointer
Struct
::New(const String& name, Access access, bool is_abstract)
{
  return new Struct(name, access, is_abstract);
}


/**
 * Construct a new Union and return a smart pointer to it.
 */
Union::Pointer
Union
::New(const String& name, Access access, bool is_abstract)
{
  return new Union(name, access, is_abstract);
}


/**
 * Construct a new QualifiedName and return a smart pointer to it.
 */
QualifiedName::Pointer
QualifiedName
::New(const String& name)
{
  return new QualifiedName(name);
}


/**
 * Construct a new NameQualifier and return a smart pointer to it.
 */
NameQualifier::Pointer
NameQualifier
::New(const String& name)
{
  return new NameQualifier(name);
}


/**
 * Construct a new BaseClass and return a smart pointer to it.
 */
BaseClass::Pointer
BaseClass
::New(Access access)
{
  return new BaseClass(access);
}


/**
 * Construct a new BaseType and return a smart pointer to it.
 */
BaseType::Pointer
BaseType
::New()
{
  return new BaseType;
}


/**
 * Construct a new UnimplementedTypeHolder and return a smart pointer to it.
 */
UnimplementedTypeHolder::Pointer
UnimplementedTypeHolder
::New()
{
  return new UnimplementedTypeHolder;
}


/**
 * Construct a new UnimplementedNameHolder and return a smart pointer to it.
 */
UnimplementedNameHolder::Pointer
UnimplementedNameHolder
::New()
{
  return new UnimplementedNameHolder;
}


const Namespace*
Context
::GetGlobalNamespace() const
{
  if(m_Context)
    { return m_Context->GetGlobalNamespace(); }
  else
    { return dynamic_cast<const Namespace*>(this); }
}  


/**
 * Given the global namespace, lookup the Class representation for this
 * BaseClass.
 */
Class::Pointer BaseClass::GetClass(const Namespace* gns)
{
  return gns->LookupClass(m_QualifiedName->Get());
}


/**
 * Get the fully qualified name of this Context.
 */
String
Context
::GetQualifiedName() const
{
  String name = this->GetName();
  const Context* c = this->GetContext().RealPointer();
  while(c && c->GetContext())
    {
    name = c->GetName() + "::" + name;
    c = c->GetContext().RealPointer();
    }
  
  return name;
}


/**
 * Get the fully qualified name of this Enumeration.
 */
String
Enumeration
::GetQualifiedName() const
{
  String name = this->GetName();
  const Context* c = this->GetContext().RealPointer();
  while(c && c->GetContext())
    {
    name = c->GetName() + "::" + name;
    c = c->GetContext().RealPointer();
    }
  
  return name;
}


/**
 * Add an enumeration value to the set of possible values.
 */
void Enumeration::AddValue(const String& name)
{
  m_Values.insert(name);
}


/**
 * Get a begin iterator to the set of possible enumeration values.
 */
Enumeration::Values::const_iterator
Enumeration::ValuesBegin() const
{
  return m_Values.begin();
}


/**
 * Get an end iterator to the set of possible enumeration values.
 */
Enumeration::Values::const_iterator
Enumeration::ValuesEnd() const
{
  return m_Values.end();
}


cxx::CvQualifiedType NamedType::GetCxxType(const Namespace* gns) const
{
  // Try looking up the name as a class type.
  String name = m_QualifiedName->Get();
  bool isConst = this->IsConst();
  bool isVolatile = this->IsVolatile();
  // If the type has no qualifiers, it may be a fundamental type.
  if(m_QualifiedName->GetTypeOfObject() == QualifiedName_id)
    {
    if(name == "unsigned char")
      {
      return CxxTypes::GetFundamentalType(cxx::FundamentalType::UnsignedChar,
                                          isConst, isVolatile);
      }
    else if((name == "unsigned short") || (name == "short unsigned int"))
      {
      return CxxTypes::GetFundamentalType(cxx::FundamentalType::UnsignedShortInt,
                                          isConst, isVolatile);
      }
    else if(name == "unsigned int")
      {
      return CxxTypes::GetFundamentalType(cxx::FundamentalType::UnsignedInt,
                                          isConst, isVolatile);
      }
    else if((name == "unsigned long") || (name == "long unsigned int"))
      {
      return CxxTypes::GetFundamentalType(cxx::FundamentalType::UnsignedLongInt,
                                          isConst, isVolatile);
      }
    else if(name == "signed char")
      {
      return CxxTypes::GetFundamentalType(cxx::FundamentalType::SignedChar,
                                          isConst, isVolatile);
      }
    else if(name == "char")
      {
      return CxxTypes::GetFundamentalType(cxx::FundamentalType::Char,
                                          isConst, isVolatile);
      }
    else if((name == "short") || (name == "signed short") || (name == "short int"))
      {
      return CxxTypes::GetFundamentalType(cxx::FundamentalType::ShortInt,
                                          isConst, isVolatile);
      }
    else if((name == "int") || (name == "signed int"))
      {
      return CxxTypes::GetFundamentalType(cxx::FundamentalType::Int,
                                          isConst, isVolatile);
      }
    else if((name == "long") || (name == "signed long") || (name == "long int"))
      {
      return CxxTypes::GetFundamentalType(cxx::FundamentalType::LongInt,
                                          isConst, isVolatile);
      }
    else if(name == "wchar_t")
      {
      return CxxTypes::GetFundamentalType(cxx::FundamentalType::WChar_t,
                                          isConst, isVolatile);
      }
    else if(name == "bool")
      {
      return CxxTypes::GetFundamentalType(cxx::FundamentalType::Bool,
                                          isConst, isVolatile);
      }
    else if(name == "float")
      {
      return CxxTypes::GetFundamentalType(cxx::FundamentalType::Float,
                                          isConst, isVolatile);
      }
    else if(name == "double")
      {
      return CxxTypes::GetFundamentalType(cxx::FundamentalType::Double,
                                          isConst, isVolatile);
      }
    else if(name == "long double")
      {
      return CxxTypes::GetFundamentalType(cxx::FundamentalType::LongDouble,
                                          isConst, isVolatile);
      }
    else if(name == "void")
      {
      return CxxTypes::GetFundamentalType(cxx::FundamentalType::Void,
                                          isConst, isVolatile);
      }
    }

  Named* result = gns->LookupName(name);
  if(result)
    {
    TypeOfObject resultType = result->GetTypeOfObject();
    if((resultType == Class_id) || (resultType == Struct_id) || (resultType == Union_id))
      {
      Class *c = dynamic_cast<Class*>(result);
      const cxx::ClassType* classType = c->GetCxxClassType(gns);
      return classType->GetCvQualifiedType(isConst, isVolatile);
      }
    else if(resultType == Enumeration_id)
      {
      Enumeration *e = dynamic_cast<Enumeration*>(result);
      const cxx::EnumerationType* enumType = e->GetCxxEnumerationType(gns);
      return enumType->GetCvQualifiedType(isConst, isVolatile);
      }
    else if(resultType == Typedef_id)
      {
      Typedef* td = dynamic_cast<Typedef*>(result);
      return td->GetOriginalType()->GetCxxType(gns);
      }
    }
  // Couldn't identify the type.
  std::cerr << "NamedType::GetCxxType()" << std::endl
            << "  ERROR: Couldn't identify type \"" << name.c_str() << "\"" << std::endl;
  return cxx::CvQualifiedType();
}

cxx::CvQualifiedType PointerType::GetCxxType(const Namespace* gns) const
{
  if(m_PointedToType->IsMethodType())
    {
    return m_PointedToType->GetCxxType(gns);
    }
  else if(m_PointedToType->IsOffsetType())
    {
    return m_PointedToType->GetCxxType(gns);
    }
  else
    {
    cxx::CvQualifiedType pointedToType = m_PointedToType->GetCxxType(gns);
    return CxxTypes::GetPointerType(pointedToType,
                                    this->IsConst(),
                                    this->IsVolatile());
    }
}

cxx::CvQualifiedType ReferenceType::GetCxxType(const Namespace* gns) const
{
  cxx::CvQualifiedType referencedType = m_ReferencedType->GetCxxType(gns);
  return CxxTypes::GetReferenceType(referencedType);
}

cxx::CvQualifiedType FunctionType::GetCxxType(const Namespace* gns) const
{
  cxx::CvQualifiedType returnType;
  cxx::CvQualifiedTypes argumentTypes;
  if(m_Returns && m_Returns->GetType())
    {
    returnType = m_Returns->GetType()->GetCxxType(gns);
    }
  else
    {
    returnType = CxxTypes::GetFundamentalType(cxx::FundamentalType::Void,
                                              false, false);
    }
  for(ArgumentContainer::const_iterator arg = m_Arguments.begin();
      arg != m_Arguments.end(); ++arg)
    {
    argumentTypes.push_back((*arg)->GetType()->GetCxxType(gns));
    }
  return CxxTypes::GetFunctionType(returnType, argumentTypes, false, false);
}

cxx::CvQualifiedType MethodType::GetCxxType(const Namespace* gns) const
{
  cxx::CvQualifiedType functionType = this->FunctionType::GetCxxType(gns);
  const cxx::ClassType* classType = gns->LookupClass(m_BaseType->GetQualifiedName())->GetCxxClassType(gns);
  return CxxTypes::GetPointerToMemberType(functionType, classType,
                                          this->IsConst(), this->IsVolatile());
}

cxx::CvQualifiedType OffsetType::GetCxxType(const Namespace* gns) const
{
  cxx::CvQualifiedType memberType = m_MemberType->GetCxxType(gns);
  const cxx::ClassType* classType = gns->LookupClass(m_BaseType->GetQualifiedName())->GetCxxClassType(gns);
  return CxxTypes::GetPointerToMemberType(memberType, classType,
                                          this->IsConst(), this->IsVolatile());
}

cxx::CvQualifiedType ArrayType::GetCxxType(const Namespace* gns) const
{
  cxx::CvQualifiedType elementType = m_ElementType->GetCxxType(gns);
  return CxxTypes::GetArrayType(elementType, m_Size);
}

const cxx::EnumerationType*
Enumeration::GetCxxEnumerationType(const Namespace* gns) const
{
  return CxxTypes::typeSystem.GetEnumerationType(this->GetQualifiedName());
}

const cxx::ClassType* Class::GetCxxClassType(const Namespace* gns) const
{
  cxx::ClassTypes baseTypes;
  for(BaseClassContainer::const_iterator base = m_BaseClasses.begin();
      base != m_BaseClasses.end(); ++base)
    {
    // Only add public base classes.
    if((*base)->GetAccess() == Public)
      {
      Class* c = (*base)->GetClass(this->GetGlobalNamespace());
      if(c)
        {
        baseTypes.push_back(c->GetCxxClassType(gns));
        }
      }
    }
  return CxxTypes::typeSystem.GetClassType(this->GetQualifiedName(),
                                           this->IsAbstract(),
                                           baseTypes);
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
Context
::ParseQualifiedName(const String& name, QualifiersInserter qualifiers) const
{  
  String qualifier = "";
  for(String::const_iterator c = name.begin(); c != name.end(); ++c)
    {
    if(*c == ':')
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
    else if(*c == '<')
      {
      // We have hit the opening of a template argument block.  We must
      // take all characters until the matching '>' character.
      qualifier.insert(qualifier.end(), '<');
      unsigned int depth = 1;
      while(++c != name.end())
        {
        qualifier.insert(qualifier.end(), *c);
        if(*c == '<')
          {
          ++depth;
          }
        else if(*c == '>')
          {
          if(--depth == 0)
            { break; }
          }
        }
      }
    else
      {
      // This is just another character.  Add it to the qualifier.
      qualifier.insert(qualifier.end(), *c);
      }
    }
  
  // Output the last qualifier.  This should never be empty unless the
  // string was empty.
  if(qualifier.length() > 0)
    *qualifiers++ = qualifier;
  
  return true;
}


/**
 * Lookup the given qualified name starting in this context.
 */
Named*
Context
::LookupName(const String& name) const
{
  // Parse the name into its qualifiers.
  Qualifiers qualifierList;
  
  if(this->ParseQualifiedName(name, std::back_inserter(qualifierList)))
    {
    // The name was valid, but may or may not exist.  Try to look it up.
    QualifiersConstIterator qBegin = qualifierList.begin();
    // Skip over empty starting qualifier if it exists and we are in the
    // global namespace.
    if((m_Context == NULL) && (qBegin != qualifierList.end()) && (qBegin->length() == 0))
      {
      ++qBegin;
      }
    return this->LookupName(qBegin, qualifierList.end());
    }
  else
    {
    std::cerr << "Couldn't parse qualified name: " << name << std::endl;
    // The name was invalid, and failed to parse.
    return NULL;
    }
}


/**
 * Lookup the given class starting in this context's scope.
 */
Class*
Context
::LookupClass(const String& name) const
{
  Named* result = this->LookupName(name);
  if(result)
    {
    TypeOfObject resultType = result->GetTypeOfObject();
    if((resultType == Class_id) || (resultType == Struct_id) || (resultType == Union_id))
      {
      Class *c = dynamic_cast<Class*>(result);
      return c;
      }
    else if(resultType == Typedef_id)
      {
      Typedef* td = dynamic_cast<Typedef*>(result);
      Class* c = td->GetClass(this->GetGlobalNamespace());
      return c;
      }
    }
  return NULL;
}


/**
 * Lookup the given name starting in this namespace's scope.
 *
 * This internal version takes iterators into a Qualifiers describing
 * the name.
 */
Named*
Context
::LookupName(QualifiersConstIterator first,
             QualifiersConstIterator last) const
{
  // If there is no name, we cannot look it up.
  if(first == last)
    {
    return NULL;
    }
  
  // Get an iterator to the second member of the list (may be the end).
  QualifiersConstIterator second = first; ++second;
  
  // Try looking up a class with that name.
  Named::Pointer key = Named::New(*first);
  DeclarationsContainer::const_iterator declsIter = m_Declarations.find(key);
  if(declsIter != m_Declarations.end())
    {
    // We have the name.
    Named* n = declsIter->RealPointer();
    if(second == last)
      {
      // This was the last qualifier.  This is the target.
      return n;
      }
    else
      {
      // Lookup the rest of the name in the nested context.
      TypeOfObject t = n->GetTypeOfObject();
      if((t == Class_id) || (t == Struct_id) || (t == Union_id)
         || (t == Namespace_id))
        {
        Context* c = dynamic_cast<Context*>(n);
        return c->LookupName(second, last);
        }
      else if(t == Typedef_id)
        {
        Typedef* td = dynamic_cast<Typedef*>(n);
        Class* c = td->GetClass(this->GetGlobalNamespace());
        if(c)
          {
          return c->LookupName(second, last);
          }
        }
      else
        {
        // Found the qualifier, but it didn't refer to another context.
        return NULL;
        }
      }
    }

  // Didn't find the first qualifier in our scope.
  return NULL;
}


/**
 * If the typedef refers to a class, struct, or union type, this returns
 * a pointer to its representation.  Otherwise, NULL is returned.
 */
Class::Pointer
Typedef
::GetClass(const Namespace* gns)
{
  if(!m_Type || (m_Type->GetTypeOfObject() != NamedType_id))
    {
    return NULL;
    }
  NamedType* nt = dynamic_cast<NamedType*>(m_Type.RealPointer());
  return gns->LookupClass(nt->GetQualifiedName()->Get());
}

} // namespace source
