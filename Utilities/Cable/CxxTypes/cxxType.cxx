/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxType.cxx
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
#include "cxxCvQualifiedType.h"

namespace _cxx_
{


/**
 * Test whether this is a TypedefType.  This is a special test because
 * TypedefType's GetRepresentationType() passes the call through to the
 * real type.  This call is not passed through.
 */
bool Type::IsTypedefType() const
{
  return false;
}


/**
 * Return the pointer that is used to identify this type.
 * For all non-typedef types, the "this" pointer is the correct value.
 */
const Type* Type::Id() const
{
  return this;
}
  

/**
 * Given cv-qualifiers, construct the CvQualifiedType referring to
 * this type.  The result may be more cv-qualified than that given if
 * this is a typedef type.
 */
CvQualifiedType
Type::GetCvQualifiedType(bool isConst, bool isVolatile) const
{
  // This is not a TypedefType.  Just construct the qualified form and
  // return it.
  return CvQualifiedType(this, isConst, isVolatile);
}


/**
 * Get the name of the type.
 */
String
Type::Name() const
{
  return this->CvName(false, false);
}


/**
 * Get the name of the type with the given cv-qualifiers.
 */
String
Type::CvName(bool isConst, bool isVolatile) const
{
  return this->GetCvQualifiedType(isConst, isVolatile).GenerateDeclaration("");
}


/**
 * Get the name of the type as it would be used in a declaration with the
 * given name.
 */
String
Type::GenerateDeclaration(const String& name) const
{
  return this->GenerateDeclaration(name, false, false);
}


/**
 * Get the name of the type as it would be used in a declaration with the
 * given name.
 */
String
Type::GenerateDeclaration(const String& name,
                          bool isConst, bool isVolatile) const
{
  return this->GenerateName(name, isConst, isVolatile);
}
  

/**
 * Get a cv-qualifier string that can be conactenated on the left end
 * of a string.
 */
String Type::GetLeftCvString(bool isConst, bool isVolatile) const
{
  if(isConst && isVolatile)
    {
    return "const volatile ";
    }
  else if(isConst)
    {
    return "const ";
    }
  else if(isVolatile)
    {
    return "volatile ";
    }
  else
    {
    return "";
    }
}


/**
 * Get a cv-qualifier string that can be conactenated on the right end
 * of a string.
 */
String Type::GetRightCvString(bool isConst, bool isVolatile) const
{
  if(isConst && isVolatile)
    {
    return " const volatile";
    }
  else if(isConst)
    {
    return " const";
    }
  else if(isVolatile)
    {
    return " volatile";
    }
  else
    {
    return "";
    }
}


/**
 * Prepare a string holding the "outer" type for concatenation to the right
 * of a named type.  This basically adds a space if the first character
 * is alphanumeric or a colon.
 */
String Type::PrepareOuterStringForPostfix(const String& outer) const
{
  if(outer.length() > 0)
    {
    char first = outer[0];
    if(((first >= 'A') && (first <= 'Z'))
       || ((first >= 'a') && (first <= 'z'))
       || ((first >= '0') && (first <= '9'))
       || (first == '_')
       || (first == ':'))
      {
      return " "+outer;
      }
    }
  return outer;
}


/**
 * Constructor that takes a RepresentationType for both the "from" and
 * "to" types of the failed cast.
 */
TypeDownCastException::TypeDownCastException(RepresentationType from,
                                             RepresentationType to):
  m_From(from),
  m_To(to)
{
}


/**
 * Constructor that automatically pulls RepresentationType out of the
 * "from" type of the failed cast.
 */
TypeDownCastException::TypeDownCastException(const Type* from,
                                             RepresentationType to):
  m_From(from->GetRepresentationType()),
  m_To(to)
{
}


/**
 * Get the exceptions text message.
 */
String TypeDownCastException::GetMessage() const
{
  static const char* representationTypeNames[] =
    {"undefined", "ArrayType", "ClassType", "PointerType",
     "PointerToMemberType", "ReferenceType", "FundamentalType",
     "FunctionType"};
  
  String from = representationTypeNames[m_From];
  String to = representationTypeNames[m_To];
  return "Attempt to cast object of type "+from+" to "+to+".";
}


} // namespace _cxx_
