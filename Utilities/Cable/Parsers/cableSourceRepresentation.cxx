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

#include <stdio.h>
#include <stdlib.h>
#include <strstream>

namespace source
{

/**
 * A singe instance of the TypeSystem will be used to register all
 * cxx type representations.  This is it.
 */
cxx::TypeSystem typeSystem;

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
::New(const String& name, Access access, bool is_static, bool is_const)
{
  return new Method(name, access, is_static, is_const);
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
::New(Access access)
{
  return new Destructor(access);
}


/**
 * Construct a new Converter and return a smart pointer to it.
 */
Converter::Pointer
Converter
::New(Access access, bool is_const)
{
  return new Converter(access, is_const);
}


/**
 * Construct a new OperatorMethod and return a smart pointer to it.
 */
OperatorMethod::Pointer
OperatorMethod
::New(const String& name, Access access, bool is_const)
{
  return new OperatorMethod(name, access, is_const);
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
::New(const String& name, Access access)
{
  return new Class(name, access);
}


/**
 * Construct a new Struct and return a smart pointer to it.
 */
Struct::Pointer
Struct
::New(const String& name, Access access)
{
  return new Struct(name, access);
}


/**
 * Construct a new Union and return a smart pointer to it.
 */
Union::Pointer
Union
::New(const String& name, Access access)
{
  return new Union(name, access);
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


/**
 * Get the string representation of the cv-qualifiers.
 */
String
CvQualifiers
::GetString() const
{
  String qualifiers = "";
  if(m_Const)
    {
    if(qualifiers.length() > 0) qualifiers += " ";
    qualifiers = "const";
    }
  if(m_Volatile)
    {
    if(qualifiers.length() > 0) qualifiers += " ";
    qualifiers += "volatile";
    }
  if(m_Restrict)
    {
    if(qualifiers.length() > 0) qualifiers += " ";
    qualifiers += "restrict";
    }
  return qualifiers;
}


/**
 * Get the string representation of the argument.
 */
String
Argument
::GetStringWithCV() const
{
  this->AssertComplete(__FILE__, __LINE__);  
  String defaultArg = "";
  if(m_Default.length() > 0)
    {
    defaultArg = "="+m_Default;
    }

  if((this->GetName().length() > 0) || (defaultArg.length() > 0))
    {
    return (m_Type->GetNameWithCV() + " " + this->GetName() + defaultArg);
    }
  else
    {
    return m_Type->GetNameWithCV();
    }
}


/**
 * Get the string representation of the argument without a cv-qualified type.
 */
String
Argument
::GetStringWithoutCV() const
{
  this->AssertComplete(__FILE__, __LINE__);  
  String defaultArg = "";
  if(m_Default.length() > 0)
    {
    defaultArg = "="+m_Default;
    }
  
  if((this->GetName().length() > 0) || (defaultArg.length() > 0))
    {
    return (m_Type->GetNameWithoutCV() + " " + this->GetName() + defaultArg);
    }
  else
    {
    return m_Type->GetNameWithoutCV();
    }
}


/**
 * Get the name of the type with cv-qualifiers.
 */
String
NamedType
::GetNameWithCV(const Type* indirect) const
{
  this->AssertComplete(__FILE__, __LINE__);
  String indirectStr = "";
  if(indirect)
    {
    indirectStr = indirect->GetIndirectionWithCV();
    }
  if(this->GetCV().length() > 0)
    {
    return (this->GetCV()+" "+m_QualifiedName->Get()+indirectStr);
    }
  else
    {
    return (m_QualifiedName->Get()+indirectStr);
    }
}


/**
 * Get the name of the type without cv-qualifiers.
 */
String
NamedType
::GetNameWithoutCV(const Type* indirect) const
{
  this->AssertComplete(__FILE__, __LINE__);
  if(indirect)
    {
    return (m_QualifiedName->Get()+indirect->GetIndirectionWithoutCV());
    }
  else
    {
    return (m_QualifiedName->Get());
    }
}


/**
 * Get the name of the pointer type with cv qualifiers.
 */
String
PointerType
::GetNameWithCV(const Type* indirect) const
{
  this->AssertComplete(__FILE__, __LINE__);
  if(indirect)
    {
    return (m_PointedToType->GetNameWithCV(this)
            + indirect->GetIndirectionWithCV());
    }
  else
    {
    return (m_PointedToType->GetNameWithCV(this));
    }
}


/**
 * Get the name of the pointer type without cv qualifiers.
 */
String
PointerType
::GetNameWithoutCV(const Type* indirect) const
{
  this->AssertComplete(__FILE__, __LINE__);
  if(indirect)
    {
    return (m_PointedToType->GetNameWithoutCV(this));
    }
  else
    {
    return (m_PointedToType->GetNameWithoutCV(this)
            + indirect->GetIndirectionWithoutCV());
    }
}


/**
 * Get the indirection string for the pointer with cv-qualifiers.
 */
String
PointerType
::GetIndirectionWithCV() const
{
  return ("*"+this->GetCV());
}


/**
 * Get the indirection string for the pointer without cv-qualifiers.
 */
String
PointerType
::GetIndirectionWithoutCV() const
{
  return ("*");
}


/**
 * Get the name of the reference type with cv-qualifiers.
 */
String
ReferenceType
::GetNameWithCV(const Type* indirect) const
{
  this->AssertComplete(__FILE__, __LINE__);
  if(indirect)
    {
    throw IndirectionOnReferenceException;
    return "";
    }
  else
    {
    return (m_ReferencedType->GetNameWithCV(this));
    }
}


/**
 * Get the name of the reference type without cv-qualifiers.
 */
String
ReferenceType
::GetNameWithoutCV(const Type* indirect) const
{
  this->AssertComplete(__FILE__, __LINE__);
  if(indirect)
    {
    throw IndirectionOnReferenceException;
    return "";
    }
  else
    {
    return (m_ReferencedType->GetNameWithoutCV(this));
    }
}


/**
 * Get the indirection string for the reference with cv-qualifiers.
 */
String
ReferenceType
::GetIndirectionWithCV() const
{
  return ("&"+this->GetCV());
}


/**
 * Get the indirection string for the reference without cv-qualifiers.
 */
String
ReferenceType
::GetIndirectionWithoutCV() const
{
  return ("&");
}


/**
 * Get the name of the function type.  The following format is used:
 * <return type>? ( <indirection/qualifiers>? ) ( <arguments> ) <qualifiers>?
 */
String
FunctionType
::GetNameWithCV(const Type* indirect) const
{
  this->AssertComplete(__FILE__, __LINE__);
  String indirectionString = "";
  if(indirect) indirectionString = indirect->GetIndirectionWithCV();
  
  String returns = "";
  if(this->GetReturns())
    {
    returns = this->GetReturns()->GetType()->GetNameWithCV() + " ";
    }
  
  String arguments = "";
  int numArgs = this->GetArgumentCount();
  for(int arg = 0; arg < numArgs-1 ; ++arg)
    {
    arguments += this->GetArgument(arg)->GetStringWithCV() + " , ";
    }
  if(numArgs > 0)
    {
    arguments += this->GetArgument(numArgs-1)->GetStringWithCV();
    }
  
  return (returns + "(" + indirectionString + ")( " + arguments + " )"
          + this->GetCV());
}


/**
 * Get the name of the function type.  The following format is used:
 * <return type>? ( <indirection>? ) ( <arguments> )
 */
String
FunctionType
::GetNameWithoutCV(const Type* indirect) const
{
  this->AssertComplete(__FILE__, __LINE__);
  String indirectionString = "";
  if(indirect) indirectionString = indirect->GetIndirectionWithoutCV();
  
  String returns = "";
  if(this->GetReturns())
    {
    returns = this->GetReturns()->GetType()->GetNameWithCV() + " ";
    }
  
  String arguments = "";
  int numArgs = this->GetArgumentCount();
  for(int arg = 0; arg < numArgs-1 ; ++arg)
    {
    arguments += this->GetArgument(arg)->GetStringWithoutCV() + " , ";
    }
  if(numArgs > 0)
    {
    arguments += this->GetArgument(numArgs-1)->GetStringWithoutCV();
    }
  
  return (returns + "(" + indirectionString + ")( " + arguments + " )");
}


/**
 * Get the name of the method type.  The following format is used:
 * <return type>? ( <base type>:: <indirection/qualifiers>? )
 *                                            ( <arguments> ) <qualifiers>?
 */
String
MethodType
::GetNameWithCV(const Type* indirect) const
{
  this->AssertComplete(__FILE__, __LINE__);
  String indirectionString = "";
  if(indirect) indirectionString = indirect->GetIndirectionWithCV();
  
  String returns = "";
  if(this->GetReturns())
    {
    returns = this->GetReturns()->GetType()->GetNameWithCV() + " ";
    }
  
  String arguments = "";
  int numArgs = this->GetArgumentCount();
  for(int arg = 0; arg < numArgs-1 ; ++arg)
    {
    arguments += this->GetArgument(arg)->GetStringWithCV() + " , ";
    }
  if(numArgs > 0)
    {
    arguments += this->GetArgument(numArgs-1)->GetStringWithCV();
    }
  
  return (returns + "(" + m_BaseType->GetQualifiedName() + "::" + indirectionString
          + ")( " + arguments + " )" + this->GetCV());
}


/**
 * Get the name of the method type.  The following format is used:
 * <return type>? ( <base type>:: <indirection>? ) ( <arguments> )
 */
String
MethodType
::GetNameWithoutCV(const Type* indirect) const
{
  this->AssertComplete(__FILE__, __LINE__);
  String indirectionString = "";
  if(indirect) indirectionString = indirect->GetIndirectionWithoutCV();
  
  String returns = "";
  if(this->GetReturns())
    {
    returns = this->GetReturns()->GetType()->GetNameWithCV() + " ";
    }
  
  String arguments = "";
  int numArgs = this->GetArgumentCount();
  for(int arg = 0; arg < numArgs-1 ; ++arg)
    {
    arguments += this->GetArgument(arg)->GetStringWithoutCV() + " , ";
    }
  if(numArgs > 0)
    {
    arguments += this->GetArgument(numArgs-1)->GetStringWithoutCV();
    }
  
  return (returns + "(" + m_BaseType->GetQualifiedName() + "::" + indirectionString
          + ")( " + arguments + " )");
}



/**
 * Get the name of the offset type (pointer to member) with cv-qualifiers.
 */
String
OffsetType
::GetNameWithCV(const Type* indirect) const
{
  this->AssertComplete(__FILE__, __LINE__);
  if(indirect)
    {
    return (m_MemberType->GetNameWithCV()+" "+m_BaseType->GetQualifiedName()+"::"
            +indirect->GetIndirectionWithCV());
    }
  else
    {
    return (m_MemberType->GetNameWithCV()+" "+m_BaseType->GetQualifiedName()+"::");
    }
}


/**
 * Get the name of the offset type (pointer to member) without cv-qualifiers.
 */
String
OffsetType
::GetNameWithoutCV(const Type* indirect) const
{
  this->AssertComplete(__FILE__, __LINE__);
  if(indirect)
    {
    return (m_MemberType->GetNameWithoutCV()+" "+m_BaseType->GetQualifiedName()+"::"
            +indirect->GetIndirectionWithoutCV());
    }
  else
    {
    return (m_MemberType->GetNameWithoutCV()+" "+m_BaseType->GetQualifiedName()+"::");
    }
}


/**
 * Get the name of the array type with cv-qualifiers.
 */
String
ArrayType
::GetNameWithCV(const Type* indirect) const
{
  this->AssertComplete(__FILE__, __LINE__);
  char sizeStr[sizeof(m_Size)*3+2];
  
  sprintf(sizeStr,"%d", m_Size);
  
  if(indirect)
    {
    return (m_ElementType->GetNameWithCV()+"["+String(sizeStr)+"]"
            +indirect->GetIndirectionWithCV());
    }
  else
    {
    return (m_ElementType->GetNameWithCV()+"["+String(sizeStr)+"]");
    }
}


/**
 * Get the name of the array type without cv-qualifiers.
 */
String
ArrayType
::GetNameWithoutCV(const Type* indirect) const
{
  this->AssertComplete(__FILE__, __LINE__);
  char sizeStr[sizeof(m_Size)*3+2];
  
  sprintf(sizeStr,"%d", m_Size);
  
  if(indirect)
    {
    return (m_ElementType->GetNameWithoutCV()+"["+String(sizeStr)+"]"
            +indirect->GetIndirectionWithoutCV());
    }
  else
    {
    return (m_ElementType->GetNameWithoutCV()+"["+String(sizeStr)+"]");
    }
}


#define XML_NESTED_INDENT 2

/**
 * Print "indent" spaces to file "file".
 */
static void PrintIndent(FILE* file, unsigned int indent)
{
  unsigned int count = indent;
  while(count >= 10)
    {
    fprintf(file, "          ");
    count -= 10;
    }
  while(count > 0)
    {
    fprintf(file, " ");
    --count;
    }
}


/**
 * Get the fully qualified name of this context.
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
 * Print out a location tag.
 */
void
Location
::Print(FILE* file, unsigned int indent) const
{
  PrintIndent(file, indent);
  fprintf(file, "<Location file=\"%s\" line=\"%d\"/>\n",
          m_File.c_str(), m_Line);
}


/**
 * Print out the classes in this context.
 */
void
Context
::PrintClasses(FILE* file, unsigned int indent) const
{
  for(ClassesIterator c = m_Classes.begin() ; c != m_Classes.end() ; ++c)
    {
    (*c)->Print(file, indent);
    }
}


/**
 * Print the namespace.
 */
void
Namespace
::Print(FILE* file, unsigned int indent) const
{
  String name = this->GetName();
  String context = "";
  
  if(this->GetContext())
    {
    context = this->GetContext()->GetQualifiedName();
    }
  
  PrintIndent(file, indent);
  fprintf(file,
          "<Namespace name=\"%s\" context=\"%s\">\n",
          name.c_str(), context.c_str());
  
  this->PrintClasses(file, indent+XML_NESTED_INDENT);

  PrintIndent(file, indent);
  fprintf(file,
          "</Namespace>\n");
}


/**
 * Print the base classes for this class.
 */
void
Class
::PrintBaseClasses(FILE* file, unsigned int indent) const
{
  for(BaseClassesIterator bc = m_BaseClasses.begin();
      bc != m_BaseClasses.end(); ++bc)
    {
    (*bc)->Print(file, indent);
    }
}


/**
 * Print the class.
 */
void
Class
::Print(FILE* file, unsigned int indent) const
{
  String name = this->GetName();
  String context = "";
  
  if(this->GetContext())
    {
    context = this->GetContext()->GetQualifiedName();
    }
  
  PrintIndent(file, indent);
  fprintf(file,
          "<Class name=\"%s\" context=\"%s\">\n",
          name.c_str(), context.c_str());
  
  this->GetLocation()->Print(file, indent+XML_NESTED_INDENT);
  this->PrintClasses(file, indent+XML_NESTED_INDENT);
  this->PrintMethods(file, indent+XML_NESTED_INDENT);
  this->PrintBaseClasses(file, indent+XML_NESTED_INDENT);
  
  PrintIndent(file, indent);
  fprintf(file,
          "</Class>\n");
}


/**
 * Print the methods in this class.
 */
void
Class
::PrintMethods(FILE* file, unsigned int indent) const
{
  for(MethodsIterator m = m_Methods.begin() ; m != m_Methods.end() ; ++m)
    {
    (*m)->Print(file, indent);
    }  
}


/**
 * Print the struct.
 */
void
Struct
::Print(FILE* file, unsigned int indent) const
{
  String name = this->GetName();
  String context = "";
  
  if(this->GetContext())
    {
    context = this->GetContext()->GetQualifiedName();
    }
  
  PrintIndent(file, indent);
  fprintf(file,
          "<Struct name=\"%s\" context=\"%s\">\n",
          name.c_str(), context.c_str());

  this->GetLocation()->Print(file, indent+XML_NESTED_INDENT);
  this->PrintClasses(file, indent+XML_NESTED_INDENT);  
  this->PrintMethods(file, indent+XML_NESTED_INDENT);
  this->PrintBaseClasses(file, indent+XML_NESTED_INDENT);
  
  PrintIndent(file, indent);
  fprintf(file,
          "</Struct>\n");
}


/**
 * Print the union.
 */
void
Union
::Print(FILE* file, unsigned int indent) const
{
  String name = this->GetName();
  String context = "";
  
  if(this->GetContext())
    {
    context = this->GetContext()->GetQualifiedName();
    }
  
  PrintIndent(file, indent);
  fprintf(file,
          "<Union name=\"%s\" context=\"%s\">\n",
          name.c_str(), context.c_str());
  
  this->GetLocation()->Print(file, indent+XML_NESTED_INDENT);
  this->PrintClasses(file, indent+XML_NESTED_INDENT);
  this->PrintMethods(file, indent+XML_NESTED_INDENT);
  this->PrintBaseClasses(file, indent+XML_NESTED_INDENT);
  
  PrintIndent(file, indent);
  fprintf(file,
          "</Union>\n");
}


/**
 * Print the CV qualifiers.
 */
void CvQualifiers::Print(FILE* file, unsigned int indent) const
{
  int is_const = m_Const? 1:0;
  int is_volatile = m_Volatile? 1:0;
  int is_restrict = m_Restrict? 1:0;
  
  PrintIndent(file, indent);
  fprintf(file,
          "<CvQualifiers const=\"%d\" volatile=\"%d\" restrict=\"%d\"/>\n",
          is_const, is_volatile, is_restrict);
}


/**
 * Print the CV qualifiers for this type.
 */
void Type::PrintCvQualifiers(FILE* file, unsigned int indent) const
{
  if(m_CvQualifiers)
    {
    m_CvQualifiers->Print(file, indent);
    }
}


/**
 * Print the name of this type.
 */
void Type::PrintName(FILE* file, unsigned int indent) const
{
  PrintIndent(file, indent);
  fprintf(file, "%s\n", this->GetName().c_str());
}


/**
 * Print out this NamedType.
 */
void NamedType::Print(FILE* file, unsigned int indent) const
{
  this->AssertComplete(__FILE__, __LINE__);
  PrintIndent(file, indent);
  fprintf(file, "<Type name=\"%s\">\n",
          this->m_QualifiedName->Get().c_str());
  this->PrintCvQualifiers(file, indent+XML_NESTED_INDENT);
  PrintIndent(file, indent);
  fprintf(file, "</Type>\n");
  this->PrintName(file, indent);
}


/**
 * Print out this PointerType.
 */
void PointerType::Print(FILE* file, unsigned int indent) const
{
  this->AssertComplete(__FILE__, __LINE__);
  PrintIndent(file, indent);
  fprintf(file, "<PointerType>\n");  
  m_PointedToType->Print(file, indent+XML_NESTED_INDENT);
  PrintIndent(file, indent);
  fprintf(file, "</PointerType>\n");
  this->PrintName(file, indent);
}


/**
 * Print out this ReferenceType.
 */
void ReferenceType::Print(FILE* file, unsigned int indent) const
{
  this->AssertComplete(__FILE__, __LINE__);
  PrintIndent(file, indent);
  fprintf(file, "<ReferenceType>\n");
  m_ReferencedType->Print(file, indent+XML_NESTED_INDENT);
  PrintIndent(file, indent);
  fprintf(file, "</ReferenceType>\n");
  this->PrintName(file, indent);
}


/**
 * Print out this FunctionType.
 */
void FunctionType::Print(FILE* file, unsigned int indent) const
{
  this->AssertComplete(__FILE__, __LINE__);
  PrintIndent(file, indent);
  fprintf(file, "<FunctionType>\n");

  if(m_Returns)
    {
    m_Returns->Print(file, indent+XML_NESTED_INDENT);
    }
  for(ArgumentsIterator a = m_Arguments.begin() ; a != m_Arguments.end() ; ++a)
    {
    (*a)->Print(file, indent+XML_NESTED_INDENT);
    }
  if(m_Ellipsis)
    {
    PrintIndent(file, indent+XML_NESTED_INDENT);
    fprintf(file, "<Ellipsis/>\n");
    }
  
  PrintIndent(file, indent);
  fprintf(file, "</FunctionType>\n");
  this->PrintName(file, indent);
}


/**
 * Print out this MethodType.
 */
void MethodType::Print(FILE* file, unsigned int indent) const
{
  this->AssertComplete(__FILE__, __LINE__);
  PrintIndent(file, indent);
  fprintf(file, "<MethodType basetype=\"??\">\n");

  this->GetReturns()->Print(file, indent+XML_NESTED_INDENT);
  for(ArgumentsIterator a = this->GetArguments().begin() ;
      a != this->GetArguments().end() ; ++a)
    {
    (*a)->Print(file, indent+XML_NESTED_INDENT);
    }
  
  if(this->GetEllipsis())
    {
    PrintIndent(file, indent+XML_NESTED_INDENT);
    fprintf(file, "<Ellipsis/>\n");
    }
  
  PrintIndent(file, indent);
  fprintf(file, "</MethodType>\n");
  this->PrintName(file, indent);
}


/**
 * Print out this OffsetType.
 */
void OffsetType::Print(FILE* file, unsigned int indent) const
{
  this->AssertComplete(__FILE__, __LINE__);
  PrintIndent(file, indent);
  fprintf(file, "<OffsetType>\n");
  m_MemberType->Print(file, indent+XML_NESTED_INDENT);
  PrintIndent(file, indent);
  fprintf(file, "</OffsetType>\n");
  this->PrintName(file, indent);
}


/**
 * Print out this ArrayType.
 */
void ArrayType::Print(FILE* file, unsigned int indent) const
{
  this->AssertComplete(__FILE__, __LINE__);
  PrintIndent(file, indent);
  fprintf(file, "<ArrayType>\n");
  m_ElementType->Print(file, indent+XML_NESTED_INDENT);
  PrintIndent(file, indent);
  fprintf(file, "</ArrayType>\n");
  this->PrintName(file, indent);
}


/**
 * Print the information for an argument.
 */
void Argument::Print(FILE* file, unsigned int indent) const
{
  this->AssertComplete(__FILE__, __LINE__);
  PrintIndent(file, indent);
  fprintf(file, "<Argument>\n");
  m_Type->Print(file, indent+XML_NESTED_INDENT);
  PrintIndent(file, indent);
  fprintf(file, "</Argument>\n");
}


/**
 * Print the information for a function return type.
 */
void Returns::Print(FILE* file, unsigned int indent) const
{
  this->AssertComplete(__FILE__, __LINE__);
  PrintIndent(file, indent);
  fprintf(file, "<Returns>\n");
  m_Type->Print(file, indent+XML_NESTED_INDENT);
  PrintIndent(file, indent);
  fprintf(file, "</Returns>\n");
}


/**
 * Print the function prototype information.  This consists of
 * the arguments, return type, and cv-qualifiers.
 */
void
Function
::PrintFunctionPrototypeInfo(FILE* file, unsigned int indent) const
{
  if(m_Returns)
    {
    m_Returns->Print(file, indent);
    }
  for(ArgumentsIterator a = m_Arguments.begin() ; a != m_Arguments.end() ; ++a)
    {
    (*a)->Print(file, indent);
    }
  if(m_Ellipsis)
    {
    PrintIndent(file, indent);
    fprintf(file, "<Ellipsis/>\n");
    }
}


/**
 * Print the function.
 */
void Function::Print(FILE* file, unsigned int indent) const
{
  String name = this->GetName();
  
  PrintIndent(file, indent);
  fprintf(file,
          "<Function name=\"%s\">\n",
          name.c_str());

  this->GetLocation()->Print(file, indent+XML_NESTED_INDENT);
  this->PrintFunctionPrototypeInfo(file, indent+XML_NESTED_INDENT);
  
  PrintIndent(file, indent);
  fprintf(file,
          "</Function>\n");
}


/**
 * Print the method.
 */
void
Method
::Print(FILE* file, unsigned int indent) const
{
  String name = this->GetName();
  
  PrintIndent(file, indent);
  fprintf(file,
          "<Method name=\"%s\">\n",
          name.c_str());

  this->GetLocation()->Print(file, indent+XML_NESTED_INDENT);
  this->PrintFunctionPrototypeInfo(file, indent+XML_NESTED_INDENT);
  
  PrintIndent(file, indent);
  fprintf(file,
          "</Method>\n");
}


/**
 * Print the constructor.
 */
void
Constructor
::Print(FILE* file, unsigned int indent) const
{
  String name = this->GetName();
  
  PrintIndent(file, indent);
  fprintf(file,
          "<Constructor>\n");

  this->GetLocation()->Print(file, indent+XML_NESTED_INDENT);
  this->PrintFunctionPrototypeInfo(file, indent+XML_NESTED_INDENT);
  
  PrintIndent(file, indent);
  fprintf(file,
          "</Constructor>\n");
}


/**
 * Print the destructor.
 */
void
Destructor
::Print(FILE* file, unsigned int indent) const
{
  String name = this->GetName();
  
  PrintIndent(file, indent);
  fprintf(file,
          "<Destructor>\n");

  this->GetLocation()->Print(file, indent+XML_NESTED_INDENT);
  this->PrintFunctionPrototypeInfo(file, indent+XML_NESTED_INDENT);
  
  PrintIndent(file, indent);
  fprintf(file,
          "</Destructor>\n");
}


/**
 * Print the converter.
 */
void
Converter
::Print(FILE* file, unsigned int indent) const
{
  String name = this->GetName();
  
  PrintIndent(file, indent);
  fprintf(file,
          "<Converter>\n");

  this->GetLocation()->Print(file, indent+XML_NESTED_INDENT);
  this->PrintFunctionPrototypeInfo(file, indent+XML_NESTED_INDENT);
  
  PrintIndent(file, indent);
  fprintf(file,
          "</Converter>\n");
}


/**
 * Print the operator method.
 */
void
OperatorMethod
::Print(FILE* file, unsigned int indent) const
{
  String name = this->GetName();
  
  PrintIndent(file, indent);
  fprintf(file,
          "<OperatorMethod name=\"%s\">\n",
          name.c_str());

  this->GetLocation()->Print(file, indent+XML_NESTED_INDENT);
  this->PrintFunctionPrototypeInfo(file, indent+XML_NESTED_INDENT);
  
  PrintIndent(file, indent);
  fprintf(file,
          "</OperatorMethod>\n");
}


/**
 * Print the operator function.
 */
void
OperatorFunction
::Print(FILE* file, unsigned int indent) const
{
  String name = this->GetName();
  
  PrintIndent(file, indent);
  fprintf(file,
          "<OperatorFunction name=\"%s\">\n",
          name.c_str());

  this->GetLocation()->Print(file, indent+XML_NESTED_INDENT);
  this->PrintFunctionPrototypeInfo(file, indent+XML_NESTED_INDENT);
  
  PrintIndent(file, indent);
  fprintf(file,
          "</OperatorFunction>\n");
}


/**
 * Print the information for this base class.
 */
void
BaseClass
::Print(FILE* file, unsigned int indent) const
{
  String access;
  
  if(m_Access == Private) access = "private";
  else if(m_Access == Protected) access = "protected";
  else access = "public";
  
  PrintIndent(file, indent);
  fprintf(file,
          "<BaseClass access=\"%s\">\n",
          access.c_str());

  // TODO: Print qualified name.
  
  PrintIndent(file, indent);
  fprintf(file,
          "</BaseClass>\n");
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
 * Lookup the given class starting in this namespace's scope.
 */
Class*
Namespace
::LookupClass(const String& name) const
{
  // Parse the name into its qualifiers.
  Qualifiers qualifierList;
  
  if(this->ParseQualifiedName(name, std::back_inserter(qualifierList)))
    {
    // The name was valid, but may or may not exist.  Try to look it up.
    Context* result = this->LookupName(qualifierList.begin(), qualifierList.end());
    if(result->IsClass() || result->IsStruct() || result->IsUnion())
      {
      return dynamic_cast<Class*>(result);
      }
    else
      {
      return NULL;
      }
    }
  else
    {
    // The name was invalid, and failed to parse.
    return NULL;
    }
}


/**
 * Lookup the given name starting in this namespace's scope.
 *
 * This internal version takes iterators into a Qualifiers describing
 * the name.
 */
Context*
Namespace
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
  
  // Try to look up the highest level qualifier in this namespace.
  Namespace::Pointer nsKey = Namespace::New(*first);
  
  NamespaceContainer::const_iterator namespaceIter = m_Namespaces.find(nsKey);
  if(namespaceIter != m_Namespaces.end())
    {
    // We have found a nested namespace.
    Namespace* ns = namespaceIter->RealPointer();
    if(second == last)
      {
      // This was the last qualifier.  This is the target.
      return ns;
      }
    else
      {
      // Lookup the rest of the name in the nested namespace.
      return ns->LookupName(second, last);
      }
    }
  
  // There was no namespace with the given name.  Try looking up
  // a class with that name.
  Class::Pointer classKey = Class::New(*first, Public);
  ClassContainer::const_iterator classIter = m_Classes.find(classKey);
  if(classIter != m_Classes.end())
    {
    // We have found a class.
    Class* c = classIter->RealPointer();
    if(second == last)
      {
      // This was the last qualifier.  This is the target.
      return c;
      }
    else
      {
      // Lookup the rest of the name in the class.
      return c->LookupName(second, last);
      }
    }
  
  // Didn't find the first qualifier in our scope.
  return NULL;
}



/**
 * Lookup the given name starting in this class's scope.
 *
 * This internal version takes iterators into a Qualifiers describing
 * the name.
 */
Context*
Class
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
  
  // Try to look up the highest level qualifier in this class.
  Class::Pointer classKey = Class::New(*first, Public);
  
  ClassContainer::const_iterator classIter = m_Classes.find(classKey);
  if(classIter != m_Classes.end())
    {
    // We have found a class.
    Class* c = classIter->RealPointer();
    if(second == last)
      {
      // This was the last qualifier.  This is the target.
      return c;
      }
    else
      {
      // Lookup the rest of the name in the class.
      return c->LookupName(second, last);
      }
    }
  
  // Didn't find the first qualifier in our scope.
  return NULL;
}


} // namespace source
