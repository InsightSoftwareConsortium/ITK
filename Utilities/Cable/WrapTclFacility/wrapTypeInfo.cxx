/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapTypeInfo.cxx
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
#include "wrapTypeInfo.h"

namespace _wrap_
{

// A macro to generate all cv-qualifier combinations for defining a
// fundamental type's CvPredefinedType<> specialization.
#define _wrap_DEFINE_FUNDAMENTAL_CVTYPES(T) \
CvQualifiedType CvPredefinedType<T>::type; \
CvQualifiedType CvPredefinedType<const T>::type; \
CvQualifiedType CvPredefinedType<volatile T>::type; \
CvQualifiedType CvPredefinedType<const volatile T>::type

/*@{
 * The actual CvQualifiedType instance for this fundamental type.
 */
#ifdef _wrap_NO_CV_VOID
CvQualifiedType CvPredefinedType<void>::type;
#else
_wrap_DEFINE_FUNDAMENTAL_CVTYPES(void);
#endif
#ifndef _wrap_NO_WCHAR_T
_wrap_DEFINE_FUNDAMENTAL_CVTYPES(wchar_t);
#endif
_wrap_DEFINE_FUNDAMENTAL_CVTYPES(bool);
_wrap_DEFINE_FUNDAMENTAL_CVTYPES(char);
_wrap_DEFINE_FUNDAMENTAL_CVTYPES(signed char);
_wrap_DEFINE_FUNDAMENTAL_CVTYPES(unsigned char);
_wrap_DEFINE_FUNDAMENTAL_CVTYPES(short);
_wrap_DEFINE_FUNDAMENTAL_CVTYPES(unsigned short);
_wrap_DEFINE_FUNDAMENTAL_CVTYPES(int);
_wrap_DEFINE_FUNDAMENTAL_CVTYPES(unsigned int);
_wrap_DEFINE_FUNDAMENTAL_CVTYPES(long);
_wrap_DEFINE_FUNDAMENTAL_CVTYPES(unsigned long);
_wrap_DEFINE_FUNDAMENTAL_CVTYPES(float);
_wrap_DEFINE_FUNDAMENTAL_CVTYPES(double);
_wrap_DEFINE_FUNDAMENTAL_CVTYPES(long double);
_wrap_DEFINE_FUNDAMENTAL_CVTYPES(char*);
//@}

/*@{
 * The actual CvQualifiedType instance for this Tcl-related type.
 */
CvQualifiedType CvPredefinedType<Tcl_Interp>::type;
CvQualifiedType CvPredefinedType<Tcl_Interp*>::type;
//@}
  

/**
 * Register an ArrayType having the given element type and size.
 */
CvQualifiedType
TypeInfo::GetArrayType(const CvQualifiedType& elementType,
                       unsigned long size)
{
  return typeSystem.GetArrayType(elementType, size)
    ->GetCvQualifiedType(false, false);
}


/**
 * Register a ClassType having the given name, cv-qualifiers, and
 * (optionally) parents.
 */
CvQualifiedType
TypeInfo::GetClassType(const String& name,
                       bool isConst, bool isVolatile,
                       bool isAbstract, const ClassTypes& parents)
{
  return typeSystem.GetClassType(name, isAbstract, parents)
    ->GetCvQualifiedType(isConst, isVolatile);
}


/**
 * Register an EnumerationType having the given name and cv-qualifiers.
 */
CvQualifiedType
TypeInfo::GetEnumerationType(const String& name,
                             bool isConst, bool isVolatile)
{
  return typeSystem.GetEnumerationType(name)
    ->GetCvQualifiedType(isConst, isVolatile);
}


/**
 * Register a FunctionType having the given return type, argument types,
 * and cv-qualifiers.
 */
CvQualifiedType
TypeInfo::GetFunctionType(const CvQualifiedType& returnType,
                          const CvQualifiedTypes& argumentTypes,
                          bool isConst, bool isVolatile)
{
  return typeSystem.GetFunctionType(returnType, argumentTypes)
    ->GetCvQualifiedType(isConst, isVolatile);
}


/**
 * Register a FundamentalType having the given type id and cv-qualifiers.
 */
CvQualifiedType
TypeInfo::GetFundamentalType(FundamentalType::Id id,
                                bool isConst, bool isVolatile)
{
#ifdef _wrap_NO_WCHAR_T
  if(id == FundamentalType::WChar_t) { id = FundamentalType::UnsignedShortInt; }
#endif
  return typeSystem.GetFundamentalType(id)
    ->GetCvQualifiedType(isConst, isVolatile);
}


/**
 * Register a PointerType pointing to the given type and having the
 * given cv-qualifiers.
 */
CvQualifiedType
TypeInfo::GetPointerType(const CvQualifiedType& referencedType,
                         bool isConst, bool isVolatile)
{
  return typeSystem.GetPointerType(referencedType)
    ->GetCvQualifiedType(isConst, isVolatile);
}


/**
 * Register a PointerToMemberType pointing to the given type inside the
 * given ClassType and having the given cv-qualifiers.
 */
CvQualifiedType
TypeInfo::GetPointerToMemberType(const CvQualifiedType& referencedType,
                                 const ClassType* classScope,
                                 bool isConst, bool isVolatile)
{
  return typeSystem.GetPointerToMemberType(referencedType, classScope)
    ->GetCvQualifiedType(isConst, isVolatile);
}


/**
 * Register a ReferenceType referencing the given type.
 */
CvQualifiedType
TypeInfo::GetReferenceType(const CvQualifiedType& referencedType)
{
  return typeSystem.GetReferenceType(referencedType)
    ->GetCvQualifiedType(false, false);
}


/**
 * There is exactly one TypeSystem in the wrapper facility.
 * This is it.
 */
TypeSystem TypeInfo::typeSystem;

  
// A macro to generate all cv-qualifier combinations for generating a
// fundamental type's CvPredefinedType<> specialization.
#define _wrap_GENERATE_FUNDAMENTAL_CVTYPES(T, ID) \
CvPredefinedType<T>::type = GetFundamentalType(FundamentalType::ID, false, false); \
CvPredefinedType<const T>::type = GetFundamentalType(FundamentalType::ID, true, false); \
CvPredefinedType<volatile T>::type = GetFundamentalType(FundamentalType::ID, false, true); \
CvPredefinedType<const volatile T>::type = GetFundamentalType(FundamentalType::ID, true, true)

/**
 * Initialization function for TypeInfo class.
 * This will be called exactly once by WrapperFacility::ClassInitialize().
 */
void TypeInfo::ClassInitialize()
{
#ifdef _wrap_NO_CV_VOID
  CvPredefinedType<void>::type = GetFundamentalType(FundamentalType::Void, false, false);
#else
  _wrap_GENERATE_FUNDAMENTAL_CVTYPES(void, Void);
#endif
#ifndef _wrap_NO_WCHAR_T
  _wrap_GENERATE_FUNDAMENTAL_CVTYPES(wchar_t, WChar_t);
#endif
  _wrap_GENERATE_FUNDAMENTAL_CVTYPES(bool, Bool);
  _wrap_GENERATE_FUNDAMENTAL_CVTYPES(char, Char);
  _wrap_GENERATE_FUNDAMENTAL_CVTYPES(signed char, SignedChar);
  _wrap_GENERATE_FUNDAMENTAL_CVTYPES(unsigned char, UnsignedChar);
  _wrap_GENERATE_FUNDAMENTAL_CVTYPES(short, ShortInt);
  _wrap_GENERATE_FUNDAMENTAL_CVTYPES(unsigned short, UnsignedShortInt);
  _wrap_GENERATE_FUNDAMENTAL_CVTYPES(int, Int);
  _wrap_GENERATE_FUNDAMENTAL_CVTYPES(unsigned int, UnsignedInt);
  _wrap_GENERATE_FUNDAMENTAL_CVTYPES(long, LongInt);
  _wrap_GENERATE_FUNDAMENTAL_CVTYPES(unsigned long, UnsignedLongInt);
  _wrap_GENERATE_FUNDAMENTAL_CVTYPES(float, Float);
  _wrap_GENERATE_FUNDAMENTAL_CVTYPES(double, Double);
  _wrap_GENERATE_FUNDAMENTAL_CVTYPES(long double, LongDouble);
  CvPredefinedType<Tcl_Interp>::type  = GetClassType("Tcl_Interp", false, false);
  CvPredefinedType<Tcl_Interp*>::type  = GetPointerType(CvPredefinedType<Tcl_Interp>::type, false, false);
  CvPredefinedType<char*>::type  = GetPointerType(CvPredefinedType<char>::type, false, false);
  CvPredefinedType<const char*>::type  = GetPointerType(CvPredefinedType<const char>::type, false, false);
  CvPredefinedType<volatile char*>::type  = GetPointerType(CvPredefinedType<volatile char>::type, false, false);
  CvPredefinedType<const volatile char*>::type  = GetPointerType(CvPredefinedType<const volatile char>::type, false, false);
}

} // namespace _wrap_
