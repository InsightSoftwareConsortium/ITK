/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapUtils.h
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
#ifndef _wrapUtils_h
#define _wrapUtils_h

// Include the C++ type representation classes.
// This also includes cxxUtils.h which does some work for us.
#include "cxxTypeSystem.h"
#include "cxxConversions.h"

/**
 * Setup a few utilities used by almost all source files.  There are some
 * differences between UNIX and Win32 platforms that are addressed here.
 */
#if defined(_WIN32) || defined(WIN32) /* Win32 version */
#  ifdef _wrap_SHARED
#    ifdef WrapTclFacility_EXPORTS
#      define _wrap_EXPORT __declspec(dllexport)
#    else
#      define _wrap_EXPORT __declspec(dllimport)
#    endif
#  else
#    define _wrap_EXPORT
#  endif
#  define WRAPPER_EXPORT __declspec(dllexport)
#else /* UNIX version */
#  define _wrap_EXPORT
#  define WRAPPER_EXPORT
#endif

// Visual C++ can't distinguish void types with different cv qualifiers.
#ifdef _MSC_VER
#define _wrap_NO_CV_VOID
#endif

// Both Visual C++ and the Intel C/C++ compiler define wchar_t as
// unsigned short.  Type information will fall through to unsinged short
// if wchar_t is used, so we can't let the information be duplicated.
#if defined(_MSC_VER) || defined(__ICL)
#define _wrap_NO_WCHAR_T
#endif

// GCC and MipsPro won't let static_cast add a const qualifier.
#if defined(__GNUC__) || defined(__sgi)
#define _wrap_CONST_CAST_HACK
#endif

// Include Tcl headers.
#include <tcl.h>

namespace _wrap_
{
// Some functions to make Tcl type checking easy.
_wrap_EXPORT bool TclObjectTypeIsNULL(Tcl_Obj*);
_wrap_EXPORT bool TclObjectTypeIsBoolean(Tcl_Obj*);
_wrap_EXPORT bool TclObjectTypeIsInt(Tcl_Obj*);
_wrap_EXPORT bool TclObjectTypeIsLong(Tcl_Obj*);
_wrap_EXPORT bool TclObjectTypeIsDouble(Tcl_Obj*);
_wrap_EXPORT bool TclObjectTypeIsString(Tcl_Obj*);
_wrap_EXPORT bool TclObjectTypeIsCmdName(Tcl_Obj*);

/**
 * The String type defined in the _cxx_ namespace.
 */
typedef ::_cxx_::String String;

/*@{
 * Pull this representation type out of _cxx_ namespace into _wrap_ namespace.
 */
typedef ::_cxx_::Type                 Type;
  
typedef ::_cxx_::ArrayType            ArrayType;
typedef ::_cxx_::ClassType            ClassType;
typedef ::_cxx_::FunctionType         FunctionType;
typedef ::_cxx_::FundamentalType      FundamentalType;
typedef ::_cxx_::PointerType          PointerType;
typedef ::_cxx_::PointerToMemberType  PointerToMemberType;
typedef ::_cxx_::ReferenceType        ReferenceType;

typedef ::_cxx_::CvQualifiedType      CvQualifiedType;
typedef ::_cxx_::TypeSystem           TypeSystem;

typedef ::_cxx_::CvQualifiedTypes     CvQualifiedTypes;
typedef ::_cxx_::ClassTypes           ClassTypes;
  
typedef ::_cxx_::Conversions          Conversions;
//@}

  
/**
 * Comparison function object for sorting based on void pointer type.
 */
struct VoidPointerCompare
{
  bool operator()(const void* l, const void* r) const
    {
    return (reinterpret_cast<unsigned long>(l)
            < reinterpret_cast<unsigned long>(r));
    }
};

} // namespace _wrap_


#endif
