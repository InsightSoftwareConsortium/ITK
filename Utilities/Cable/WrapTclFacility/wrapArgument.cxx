/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapArgument.cxx
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
#include "wrapArgument.h"
#include "wrapTypeInfo.h"

namespace _wrap_
{

/**
 * Constructor just sets argument as uninitialized.
 */
Argument::Argument():
  m_ArgumentId(Uninitialized_id)
{
}


/**
 * Copy constructor ensures m_Anything still points to the right place.
 */
Argument::Argument(const Argument& a):
  m_Anything(a.m_Anything),
  m_Type(a.m_Type),
  m_ArgumentId(a.m_ArgumentId),
  m_Temp(a.m_Temp)
{
  // Make sure m_Anything points to the right place in the copy.
  switch (m_ArgumentId)
    {
    case bool_id:    m_Anything.object = &m_Temp.m_bool; break;
    case int_id:     m_Anything.object = &m_Temp.m_int; break;
    case long_id:    m_Anything.object = &m_Temp.m_long; break;
    case double_id:  m_Anything.object = &m_Temp.m_double; break;
    case Object_id:
    case Pointer_id:
    case Function_id:
    case Uninitialized_id:
    default: break;
    }
}


/**
 * Assignment operator just duplicates functionality of copy constructor.
 */
Argument& Argument::operator=(const Argument& a)
{
  // Copy the values.
  m_Anything = a.m_Anything;
  m_Type = a.m_Type;
  m_ArgumentId = a.m_ArgumentId;
  m_Temp = a.m_Temp;
  
  // Make sure m_Anything points to the right place in the copy.
  switch (m_ArgumentId)
    {
    case bool_id:    m_Anything.object = &m_Temp.m_bool; break;
    case int_id:     m_Anything.object = &m_Temp.m_int; break;
    case long_id:    m_Anything.object = &m_Temp.m_long; break;
    case double_id:  m_Anything.object = &m_Temp.m_double; break;
    case Object_id:
    case Pointer_id:
    case Function_id:
    case Uninitialized_id:
    default: break;
    }
  
  return *this;
}


/**
 * Get the value of the Argument for passing to the conversion function.
 */
Anything Argument::GetValue() const
{
  // TODO: Throw exception for uninitalized argument.
  return m_Anything;
}


/**
 * Get the type of the Argument for selecting a conversion function.
 */
const CvQualifiedType& Argument::GetType() const
{
  // TODO: Throw exception for uninitalized argument.
  return m_Type;
}


/**
 * Set the type of the Argument for selecting a conversion function.
 * This should only be used to dereference the implicit object argument.
 */
void Argument::SetType(const CvQualifiedType& type)
{
  m_Type = type;
}


/**
 * Set the Argument to be the object pointed to by the given pointer.
 */
void Argument::SetToObject(ObjectType object,
                           const CvQualifiedType& type)
{
  m_Anything.object = object;
  m_Type = type;
  m_ArgumentId = Object_id;
}


/**
 * Set the Argument to be the given bool value.
 */
void Argument::SetToBool(bool b)
{
  m_Temp.m_bool = b;
  m_Anything.object = &m_Temp.m_bool;
  m_Type = CvPredefinedType<bool>::type;
  m_ArgumentId = bool_id;
}


/**
 * Set the Argument to be the given int value.
 */
void Argument::SetToInt(int i)
{
  m_Temp.m_int = i;
  m_Anything.object = &m_Temp.m_int;
  m_Type = CvPredefinedType<int>::type;
  m_ArgumentId = int_id;
}


/**
 * Set the Argument to be the given long value.
 */
void Argument::SetToLong(long l)
{
  m_Temp.m_long = l;
  m_Anything.object = &m_Temp.m_long;
  m_Type = CvPredefinedType<long>::type;
  m_ArgumentId = long_id;
}


/**
 * Set the Argument to be the given double value.
 */
void Argument::SetToDouble(double d)
{
  m_Temp.m_double = d;
  m_Anything.object = &m_Temp.m_double;
  m_Type = CvPredefinedType<double>::type;
  m_ArgumentId = double_id;
}


/**
 * Set the Argument to be the given pointer value.
 */
void Argument::SetToPointer(ObjectType v,
                            const CvQualifiedType& pointerType)
{
  m_Anything.object = v;
  m_Type = pointerType;
  m_ArgumentId = Pointer_id;
}


/**
 * Set the Argument to be the given function pointer value.
 */
void Argument::SetToFunction(FunctionType f,
                             const CvQualifiedType& functionPointerType)
{
  m_Anything.function = f;
  m_Type = functionPointerType;
  m_ArgumentId = Function_id;
}

} // namespace _wrap_
