/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapTypeInfo.h
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
                                      bool isAbstract = false,
                                      const ClassTypes& parents = ClassTypes());
  static CvQualifiedType GetEnumerationType(const String& name,
                                            bool isConst, bool isVolatile);
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
 * The wrapper facility uses specializations of this class with the member
 * static CvQualifiedType name;
 * for each type it needs to use.
 */
template <class T>
struct CvPredefinedType;

// A macro to generate all cv-qualifier combinations for declaring a
// fundamental type's CvPredefinedType<> specialization.
#define _wrap_DECLARE_FUNDAMENTAL_CVTYPES(T) \
template <> struct _wrap_EXPORT CvPredefinedType<T> { static CvQualifiedType type; }; \
template <> struct _wrap_EXPORT CvPredefinedType<const T> { static CvQualifiedType type; }; \
template <> struct _wrap_EXPORT CvPredefinedType<volatile T> { static CvQualifiedType type; }; \
template <> struct _wrap_EXPORT CvPredefinedType<const volatile T> { static CvQualifiedType type; }

/*@{
 * A specialization for a fundamental type.
 */
  
// These require some special handling for some compilers.
#ifdef _wrap_NO_CV_VOID
template <> struct _wrap_EXPORT CvPredefinedType<void> { static CvQualifiedType type; };
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

/*@{
 * A specialization for a Tcl-related type.
 */
template <> struct _wrap_EXPORT CvPredefinedType<Tcl_Interp> { static CvQualifiedType type; };
template <> struct _wrap_EXPORT CvPredefinedType<Tcl_Interp*> { static CvQualifiedType type; };
//@}
  
} // namespace _wrap_

#endif
