/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxTypeSystem.h
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
#ifndef _cxxTypeSystem_h
#define _cxxTypeSystem_h

// Include all the type representations.
#include "cxxTypes.h"

#include <map>

namespace _cxx_
{

/**
 * A complete system of types.  This class must be used to generate all
 * type representations.
 */
class _cxx_EXPORT TypeSystem
{
public:

  const ArrayType*           GetArrayType(const CvQualifiedType&,
                                          unsigned long);
  ClassType*                 GetClassType(const String&,
                                          bool isAbstract = false,
                                          const ClassTypes& = ClassTypes());
  const EnumerationType*     GetEnumerationType(const String&);
  const FunctionType*        GetFunctionType(const CvQualifiedType&,
                                             const CvQualifiedTypes&);
  const FundamentalType*     GetFundamentalType(FundamentalType::Id);
  const PointerType*         GetPointerType(const CvQualifiedType&);
  const PointerToMemberType* GetPointerToMemberType(const CvQualifiedType&,
                                              const ClassType*);
  const ReferenceType*       GetReferenceType(const CvQualifiedType&);
  
  ~TypeSystem();
  
private:
  /**
   * The key type for the ArrayTypeMap.
   */
  typedef std::pair<CvQualifiedType, unsigned long>  ArrayTypeKey;  

  /**
   * Map from this type's identifying properties to the instance.
   */
  typedef std::map<ArrayTypeKey, ArrayType*>  ArrayTypeMap;
  
  /**
   * Store all the ArrayType instances that have been allocated.
   */
  ArrayTypeMap m_ArrayTypeMap;
  
  /**
   * The key type for the ClassTypeMap.
   */
  typedef String ClassTypeKey; 
  
  /**
   * Map from this type's identifying properties to the instance.
   */
  typedef std::map<ClassTypeKey, ClassType*>  ClassTypeMap;
  
  /**
   * Store all the ClassType instances that have been allocated.
   */
  ClassTypeMap m_ClassTypeMap;
  
  /**
   * The key type for the EnumerationTypeMap.
   */
  typedef String EnumerationTypeKey; 
  
  /**
   * Map from this type's identifying properties to the instance.
   */
  typedef std::map<EnumerationTypeKey, EnumerationType*>  EnumerationTypeMap;
  
  /**
   * Store all the EnumerationType instances that have been allocated.
   */
  EnumerationTypeMap m_EnumerationTypeMap;

  /**
   * The key type for the FunctionTypeMap.
   */
  typedef std::pair<CvQualifiedType, CvQualifiedTypes>  FunctionTypeKey;
  
  /**
   * Map from a function's return type and argument types to its
   * FunctionType representation.
   */
  typedef std::map<FunctionTypeKey, FunctionType*> FunctionTypeMap;

  /**
   * Store all the FunctionType instances that have been allocated.
   */
  FunctionTypeMap m_FunctionTypeMap;
  
  /**
   * The key type for the FundamentalTypeMap.
   */
  typedef FundamentalType::Id FundamentalTypeKey;

  /**
   * Map from this type's identifying properties to the instance.
   */
  typedef std::map<FundamentalTypeKey, FundamentalType*>  FundamentalTypeMap;
  
  /**
   * Store all the FundamentalType instances that have been allocated.
   */
  FundamentalTypeMap m_FundamentalTypeMap;

  /**
   * The key type for the PointerTypeMap.
   */
  typedef CvQualifiedType PointerTypeKey;
  
  /**
   * Map from this type's identifying properties to the instance.
   */
  typedef std::map<PointerTypeKey, PointerType*>  PointerTypeMap;

  /**
   * Store all the PointerType instances that have been allocated.
   */
  PointerTypeMap m_PointerTypeMap;

  /**
   * The key type for the PointerToMemberTypeMap.
   */
  typedef std::pair<CvQualifiedType, const ClassType*>  PointerToMemberTypeKey;

  /**
   * Map from this type's identifying properties to the instance.
   */
  typedef std::map<PointerToMemberTypeKey, PointerToMemberType*>
          PointerToMemberTypeMap;
  /**
   * Store all the PointerToMemberType instances that have been allocated.
   */
  PointerToMemberTypeMap m_PointerToMemberTypeMap;
  
  /**
   * The key type for the ReferenceTypeMap.
   */
  typedef CvQualifiedType ReferenceTypeKey;
  
  /**
   * Map from this type's identifying properties to the instance.
   */
  typedef std::map<ReferenceTypeKey, ReferenceType*>  ReferenceTypeMap;

  /**
   * Store all the ReferenceType instances that have been allocated.
   */
  ReferenceTypeMap m_ReferenceTypeMap;  
};

} // namespace _cxx_

#endif

