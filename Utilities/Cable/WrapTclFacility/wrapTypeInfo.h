/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapTypeInfo.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _wrapTypeInfo_h
#define _wrapTypeInfo_h

#include "wrapUtils.h"

namespace _wrap_
{


/**
 * A class to maintain a single instance of TypeSystem.
 */
class _wrap_EXPORT TypeInfo
{
public:
  static void Initialize();

  static CvQualifiedType GetArrayType(const CvQualifiedType& elementType,
                                      unsigned long size);
  static CvQualifiedType GetClassType(const String& name,
                                      bool isConst, bool isVolatile,
                                      const ClassTypes& parents = ClassTypes());
  static CvQualifiedType GetFunctionType(const CvQualifiedType& returnType,
                                         const CvQualifiedTypes& argumentTypes,
                                         bool isConst, bool isVolatile);
  static CvQualifiedType GetFundamentalType(FundamentalType::Id,
                                            bool isConst, bool isVolatile);
  static CvQualifiedType GetPointerType(const CvQualifiedType& referencedType,
                                        bool isConst, bool isVolatile);
  static CvQualifiedType GetPointerToMemberType(const CvQualifiedType& referencedType,
                                         const ClassType* classScope,
                                         bool isConst, bool isVolatile);
  static CvQualifiedType GetReferenceType(const CvQualifiedType& referencedType);
private:
  static TypeSystem typeSystem;
};

/**
 * Every type invovled in a wrapper should have a specialization of this
 * class with the following member:
 * static CvQualifiedType name;
 */
template <class T>
struct CvType;

// A macro to generate all cv-qualifier combinations for declaring a
// fundamental type's CvType<> specialization.
#define _wrap_DECLARE_FUNDAMENTAL_CVTYPES(T) \
template <> struct _wrap_EXPORT CvType<T> { static CvQualifiedType type; }; \
template <> struct _wrap_EXPORT CvType<const T> { static CvQualifiedType type; }; \
template <> struct _wrap_EXPORT CvType<volatile T> { static CvQualifiedType type; }; \
template <> struct _wrap_EXPORT CvType<const volatile T> { static CvQualifiedType type; }

/*@{
 * A specialization for a fundamental type.
 */
  
// These require some special handling for some compilers.
#ifdef _wrap_NO_CV_VOID
template <> struct _wrap_EXPORT CvType<void> { static CvQualifiedType type; };
#else  
_wrap_DECLARE_FUNDAMENTAL_CVTYPES(void);
#endif
#ifndef _wrap_NO_WCHAR_T
_wrap_DECLARE_FUNDAMENTAL_CVTYPES(wchar_t);
#endif
  
// Normal fundamental type definitions.
_wrap_DECLARE_FUNDAMENTAL_CVTYPES(bool);
_wrap_DECLARE_FUNDAMENTAL_CVTYPES(char);
_wrap_DECLARE_FUNDAMENTAL_CVTYPES(signed char);
_wrap_DECLARE_FUNDAMENTAL_CVTYPES(unsigned char);
_wrap_DECLARE_FUNDAMENTAL_CVTYPES(short);
_wrap_DECLARE_FUNDAMENTAL_CVTYPES(unsigned short);
_wrap_DECLARE_FUNDAMENTAL_CVTYPES(int);
_wrap_DECLARE_FUNDAMENTAL_CVTYPES(unsigned int);
_wrap_DECLARE_FUNDAMENTAL_CVTYPES(long);
_wrap_DECLARE_FUNDAMENTAL_CVTYPES(unsigned long);
_wrap_DECLARE_FUNDAMENTAL_CVTYPES(float);
_wrap_DECLARE_FUNDAMENTAL_CVTYPES(double);
_wrap_DECLARE_FUNDAMENTAL_CVTYPES(long double);
_wrap_DECLARE_FUNDAMENTAL_CVTYPES(char*);
//@}  

// We are done with this macro.  Don't let it leak out of this header.
#undef _wrap_DECLARE_FUNDAMENTAL_CVTYPES  
  
} // namespace _wrap_

#endif
