/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapTypeInfo.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#include "wrapTypeInfo.h"

namespace _wrap_
{

// A macro to generate all cv-qualifier combinations for defining a
// fundamental type's CvType<> specialization.
#define _wrap_DEFINE_FUNDAMENTAL_CVTYPES(T) \
CvQualifiedType CvType<T>::type; \
CvQualifiedType CvType<const T>::type; \
CvQualifiedType CvType<volatile T>::type; \
CvQualifiedType CvType<const volatile T>::type

/*@{
 * The actual CvQualifiedType instance for this fundamental type.
 */
#ifdef _wrap_NO_CV_VOID
CvQualifiedType CvType<void>::type;
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

  
/**
 * There is exactly one TypeSystem in the wrapper facility.
 * This is it.
 */
TypeSystem TypeInfo::typeSystem;

  
// A macro to generate all cv-qualifier combinations for generating a
// fundamental type's CvType<> specialization.
#define _wrap_GENERATE_FUNDAMENTAL_CVTYPES(T, ID) \
CvType<T>::type = GetFundamentalType(FundamentalType::ID, false, false); \
CvType<const T>::type = GetFundamentalType(FundamentalType::ID, true, false); \
CvType<volatile T>::type = GetFundamentalType(FundamentalType::ID, false, true); \
CvType<const volatile T>::type = GetFundamentalType(FundamentalType::ID, true, true)
  
/**
 * Setup the type representations of the predefined, fundamental types.
 */
void TypeInfo::Initialize()
{
#ifdef _wrap_NO_CV_VOID
  CvType<void>::type = GetFundamentalType(FundamentalType::Void, false, false);
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
  CvType<char*>::type  = GetPointerType(CvType<char>::type, false, false);
  CvType<const char*>::type  = GetPointerType(CvType<const char>::type, false, false);
  CvType<volatile char*>::type  = GetPointerType(CvType<volatile char>::type, false, false);
  CvType<const volatile char*>::type  = GetPointerType(CvType<const volatile char>::type, false, false);
}


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
                       const ClassTypes& parents)
{
  return typeSystem.GetClassType(name, parents)
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


} // namespace _wrap_
