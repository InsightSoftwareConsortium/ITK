/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapMethod.cxx
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

#include "wrapMethod.h"
#include "wrapTypeInfo.h"
#include "wrapWrapperBase.h"

namespace _wrap_
{

/**
 * The constructor passes the function name and pararmeter types down to
 * the FunctionBase.  It then adds the implicit object parameter to the
 * front of the parameter list.
 */
Method::Method(WrapperBase* wrapper,
               MethodWrapper methodWrapper,
               const String& name,
               bool isConst,
               const CvQualifiedType& returnType,
               const ParameterTypes& parameterTypes):
  FunctionBase(name, parameterTypes),
  m_Wrapper(wrapper),
  m_MethodWrapper(methodWrapper)
{
  // Construct the function type associated with the method.  This does not
  // include the implicit object parameter.
  CvQualifiedTypes parameterTypes;  
  for(ParameterTypes::const_iterator arg = m_ParameterTypes.begin();
      arg != m_ParameterTypes.end(); ++arg)
    {
    parameterTypes.push_back((*arg)->GetCvQualifiedType(false, false));
    }
  m_FunctionType = TypeInfo::GetFunctionType(returnType, parameterTypes,
                                             isConst, false);

  // Add the implicit object parameter to the front of the parameter list.
  CvQualifiedType wrappedType = wrapper->GetWrappedTypeRepresentation()
    ->GetCvQualifiedType(isConst, false);
  const Type* implicit = TypeInfo::GetReferenceType(wrappedType).GetType();
  m_ParameterTypes.insert(m_ParameterTypes.begin(), implicit);

}

  
/**
 * Return whether the method is static.
 */
bool Method::IsStatic() const
{
  return false;
}

/**
 * Get a string representation of the method's function prototype.
 */
String Method::GetPrototype() const
{
  String name = m_Wrapper->GetWrappedTypeRepresentation()->Name()+"::"+this->GetCallName();
  String prototype = m_FunctionType.GenerateDeclaration(name);
  if(this->IsStatic())
    {
    prototype = "static "+prototype;
    }
  return prototype;
}

String Method::GetInclassPrototype() const
{
  String prototype = m_FunctionType.GenerateDeclaration(this->GetCallName());
  if(this->IsStatic())
    {
    prototype = "static "+prototype;
    }
  return prototype;
}


/**
 * Get the name of the method as it would be called with the standard
 * obj.method() syntax.  This is used to add the operator keyword to
 * the name, if necessary.
 */
String Method::GetCallName() const
{
  if(m_Name.length() == 0)
    {
    return "";
    }
  
  char ch = m_Name[0];
  if(((ch >= 'A') && (ch <= 'Z'))
     || ((ch >= 'a') && (ch <= 'z'))
     || (ch == '_'))
    {
    return m_Name;
    }
  else
    {
    return "operator"+m_Name;
    }
}


/**
 * Invokes a wrapped method.  This actually extracts the C++ objects
 * from the Tcl objects given as arguments and calls the method wrapper.
 */
void Method::Call(const Arguments& arguments) const
{
  // Call the method wrapper.
  m_MethodWrapper(m_Wrapper, arguments);
}


} // namespace _wrap_
