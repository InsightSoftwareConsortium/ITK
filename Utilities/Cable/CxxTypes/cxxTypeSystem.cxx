/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxTypeSystem.cxx
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
#include "cxxTypeSystem.h"

namespace _cxx_
{


/**
 * Get the type representation for an ArrayType with the given element type
 * and length.  If one does not exist, it will be created.
 */
const ArrayType*
TypeSystem
::GetArrayType(const CvQualifiedType& in_elementType,
               unsigned long in_length)
{
  // Create a key to identify this type in the map.
  ArrayTypeKey key(in_elementType, in_length);
  
  // Look for an existing copy of this type.
  ArrayTypeMap::const_iterator i = m_ArrayTypeMap.find(key);
  
  if(i != m_ArrayTypeMap.end())
    {
    // An existing copy was found, return it.
    return i->second;
    }
  else
    {
    // This is a new type.  Generate an entry and return it.
    ArrayType* newArrayType = new ArrayType(in_elementType, in_length);
    m_ArrayTypeMap[key] = newArrayType;
    return newArrayType;
    }
}


/**
 * Get the type representation for a ClassType with the given name
 * and superclasses.
 * If one does not exist, it will be created.
 * We allow the pointer to a non-const ClassType so that
 * conversion constructor/operator information can be added.
 */
ClassType*
TypeSystem
::GetClassType(const String& name, bool isAbstract, const ClassTypes& parents)
{
  // Look for an existing copy of this type.
  ClassTypeMap::const_iterator i = m_ClassTypeMap.find(name);
  
  if(i != m_ClassTypeMap.end())
    {
    // An existing copy was found.  Merge in the new information we have
    // and return it.
    i->second->MergeClassInformation(isAbstract, parents);
    return i->second;
    }
  else
    {
    // This is a new type.  Generate an entry and return it.
    ClassType* newClassType = new ClassType(name, isAbstract, parents);
    m_ClassTypeMap[name] = newClassType;
    return newClassType;
    }
}


/**
 * Get the type representation for a EnumerationType with the given name.
 * If one does not exist, it will be created.
 */
const EnumerationType*
TypeSystem
::GetEnumerationType(const String& name)
{
  // Look for an existing copy of this type.
  EnumerationTypeMap::const_iterator i = m_EnumerationTypeMap.find(name);
  
  if(i != m_EnumerationTypeMap.end())
    {
    // An existing copy was found, return it.
    return i->second;
    }
  else
    {
    // This is a new type.  Generate an entry and return it.
    EnumerationType* newEnumerationType = new EnumerationType(name);
    m_EnumerationTypeMap[name] = newEnumerationType;
    return newEnumerationType;
    }
}


/**
 * Get the type representation for a FunctionType with the given return
 * type and argument types.
 */
const FunctionType*
TypeSystem
::GetFunctionType(const CvQualifiedType& returnType,
                  const CvQualifiedTypes& arguments)
{
  // Create a key to identify this type in the map.
  FunctionTypeKey key(returnType, arguments);
  
  // Look for an existing copy of this type.
  FunctionTypeMap::const_iterator i = m_FunctionTypeMap.find(key);
  
  if(i != m_FunctionTypeMap.end())
    {
    // An existing copy was found, return it.
    return i->second;
    }
  else
    {
    // This is a new type.  Generate an entry and return it.
    FunctionType* newFunctionType =
      new FunctionType(returnType, arguments);
    m_FunctionTypeMap[key] = newFunctionType;
    return newFunctionType;
    }  
}


/**
 * Get the type representation for a FundamentalType with the given id.
 * If one does not exist, it will be created.
 */
const FundamentalType*
TypeSystem
::GetFundamentalType(FundamentalType::Id in_id)
{
  // Look for an existing copy of this type.
  FundamentalTypeMap::const_iterator i = m_FundamentalTypeMap.find(in_id);
  
  if(i != m_FundamentalTypeMap.end())
    {
    // An existing copy was found, return it.
    return i->second;
    }
  else
    {
    // This is a new type.  Generate an entry and return it.
    FundamentalType* newFundamentalType = new FundamentalType(in_id);
    m_FundamentalTypeMap[in_id] = newFundamentalType;
    return newFundamentalType;
    }
}


/**
 * Get the type representation for a PointerType with the given destination
 * type.  If one does not exist, it will be created.
 */
const PointerType*
TypeSystem
::GetPointerType(const CvQualifiedType& in_type)
{
  // Look for an existing copy of this type.
  PointerTypeMap::const_iterator i = m_PointerTypeMap.find(in_type);
  
  if(i != m_PointerTypeMap.end())
    {
    // An existing copy was found, return it.
    return i->second;
    }
  else
    {
    // This is a new type.  Generate an entry and return it.
    PointerType* newPointerType = new PointerType(in_type);
    m_PointerTypeMap[in_type] = newPointerType;
    return newPointerType;
    }
}


/**
 * Get the type representation for a PointerToMemberType with the given
 * destination type and ClassType.  If one does not exist, it will be created.
 */
const PointerToMemberType*
TypeSystem
::GetPointerToMemberType(const CvQualifiedType& in_type,
                            const ClassType* in_class)
{
  // Create a key to identify this type in the map.
  PointerToMemberTypeKey key(in_type, in_class);
  
  // Look for an existing copy of this type.
  PointerToMemberTypeMap::const_iterator i =
    m_PointerToMemberTypeMap.find(key);
  
  if(i != m_PointerToMemberTypeMap.end())
    {
    // An existing copy was found, return it.
    return i->second;
    }
  else
    {
    // This is a new type.  Generate an entry and return it.
    PointerToMemberType* newPointerToMemberType =
      new PointerToMemberType(in_type, in_class);
    m_PointerToMemberTypeMap[key] = newPointerToMemberType;
    return newPointerToMemberType;
    }
}


/**
 * Get the type representation for a ReferenceType with the given
 * destination type.  If one does not exist, it will be created.
 */
const ReferenceType*
TypeSystem
::GetReferenceType(const CvQualifiedType& in_type)
{
  // Look for an existing copy of this type.
  ReferenceTypeMap::const_iterator i = m_ReferenceTypeMap.find(in_type);
  
  if(i != m_ReferenceTypeMap.end())
    {
    // An existing copy was found, return it.
    return i->second;
    }
  else
    {
    // This is a new type.  Generate an entry and return it.
    ReferenceType* newReferenceType = new ReferenceType(in_type);
    m_ReferenceTypeMap[in_type] = newReferenceType;
    return newReferenceType;
    }
}


/**
 * Destructor frees all existing type representations allocated by this
 * TypeSystem.
 */
TypeSystem
::~TypeSystem()
{
  for(ArrayTypeMap::iterator i = m_ArrayTypeMap.begin();
      i != m_ArrayTypeMap.end(); ++i)
    {
    delete i->second;
    }
  for(ClassTypeMap::iterator i = m_ClassTypeMap.begin();
      i != m_ClassTypeMap.end(); ++i)
    {
    delete i->second;
    }
  for(FunctionTypeMap::iterator i = m_FunctionTypeMap.begin();
      i != m_FunctionTypeMap.end(); ++i)
    {
    delete i->second;
    }
  for(FundamentalTypeMap::iterator i = m_FundamentalTypeMap.begin();
      i != m_FundamentalTypeMap.end(); ++i)
    {
    delete i->second;
    }
  for(PointerTypeMap::iterator i = m_PointerTypeMap.begin();
      i != m_PointerTypeMap.end(); ++i)
    {
    delete i->second;
    }
  for(PointerToMemberTypeMap::iterator i = m_PointerToMemberTypeMap.begin();
      i != m_PointerToMemberTypeMap.end(); ++i)
    {
    delete i->second;
    }
  for(ReferenceTypeMap::iterator i = m_ReferenceTypeMap.begin();
      i != m_ReferenceTypeMap.end(); ++i)
    {
    delete i->second;
    }
}


} // namespace _cxx_
