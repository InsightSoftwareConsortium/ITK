/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxFunctionType.cxx
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
#include "cxxFunctionType.h"

namespace _cxx_
{

/**
 * Retrieve what kind of Type this is.
 */
RepresentationType FunctionType::GetRepresentationType() const
{
  return FunctionType_id;
}


/**
 * Try to cast the given Type to an FunctionType.  If this returns, the
 * pointer will be valid.  If the cast is not allowed, an exception is
 * thrown.
 */
FunctionType* FunctionType::SafeDownCast(Type* t)
{
  FunctionType* result = dynamic_cast<FunctionType*>(t);
  if(!result) { throw TypeDownCastException(t, FunctionType_id); }
  return result;
}


/**
 * Try to cast the given Type to an FunctionType.  If this returns, the
 * pointer will be valid.  If the cast is not allowed, an exception is
 * thrown.
 */
const FunctionType* FunctionType::SafeDownCast(const Type* t)
{
  const FunctionType* result = dynamic_cast<const FunctionType*>(t);
  if(!result) { throw TypeDownCastException(t, FunctionType_id); }
  return result;
}


String FunctionType::GenerateName(const String& outerType,
                                  bool isConst, bool) const
{
  String arguments = this->GenerateArgumentString();
  String outerString = "("+outerType+")( "+arguments+" )"+this->GetRightCvString(isConst, false);
  return m_ReturnType.GenerateName(outerString);
}


/**
 * Get the name of the type as it would be used in a declaration with the
 * given name.
 */
String FunctionType::GenerateDeclaration(const String& name,
                                         bool isConst, bool) const
{
  String arguments = this->GenerateArgumentString();
  String outerString = "("+name+"( "+arguments+" ))";
  return m_ReturnType.GenerateName(outerString);
}


/**
 * Called by GenerateName and GenerateDeclaration to prepare a string
 * holding the function type's arguments.
 */
String FunctionType::GenerateArgumentString() const
{
  String arguments = "";
  CvQualifiedTypes::const_iterator arg = m_Arguments.begin();
  if(arg != m_Arguments.end())
    {    
    arguments += arg->GetName();
    for(++arg;arg != m_Arguments.end(); ++arg)
      {
      arguments += ", "+arg->GetName();
      }
    }
  return arguments;
}


/**
 * Constructor takes the return type of the function.
 */
FunctionType::FunctionType(const CvQualifiedType& returnType,
                           const CvQualifiedTypes& arguments):
  m_ReturnType(returnType),
  m_Arguments(arguments)
{
}


} // namespace _cxx_
