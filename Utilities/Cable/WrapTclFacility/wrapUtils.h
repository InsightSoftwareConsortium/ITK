/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    wrapUtils.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _wrapUtils_h
#define _wrapUtils_h

/**
 * Disable some warnings.
 */

// Visual C++
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#pragma warning ( disable : 4251 )
#endif

// Intel C++ Compiler
#ifdef __ICL
#pragma warning ( disable : 985 )
#endif

/**
 * Setup a few utilities used by almost all source files.  There are some
 * differences between UNIX and Win32 platforms that are addressed here.
 */
#if defined(_WIN32) || defined(WIN32) /* Win32 version */

#define _wrap_IMPORT __declspec( dllimport )
#define _wrap_EXPORT __declspec( dllexport )

#include "wrapDllAllocator.h"

#include <string>

/**
 * Define the type "String" to be just like the STL "string", but with our
 * DLL-boundary-safe allocator for the Win32 version.
 */
typedef std::basic_string<char, std::char_traits<char>, DllAllocator<char> >  String;

#else /* UNIX version */

#define _wrap_IMPORT
#define _wrap_EXPORT

#include <string>

/**
 * Define the type "String" to be just like the STL "string".  In UNIX,
 * there are no problems with this in shared libraries.
 */
typedef std::string  String;

#endif

// Include Tcl headers.
#include <tcl.h>

// Include the C++ type representation classes.
#include "cxxTypes.h"

namespace _wrap_
{
// Some functions to make Tcl type checking easy.
_wrap_EXPORT bool TclObjectTypeIsNULL(Tcl_Obj*);
_wrap_EXPORT bool TclObjectTypeIsBoolean(Tcl_Obj*);
_wrap_EXPORT bool TclObjectTypeIsInt(Tcl_Obj*);
_wrap_EXPORT bool TclObjectTypeIsDouble(Tcl_Obj*);
_wrap_EXPORT bool TclObjectTypeIsString(Tcl_Obj*);
_wrap_EXPORT bool TclObjectTypeIsCmdName(Tcl_Obj*);

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
//@}

  
/**
 * Comparison function object for sorting based on pointer type.
 */
template <typename T>
struct PointerCompare
{
  bool operator()(T* l, T* r) const
    {
    return (reinterpret_cast<unsigned long>(l)
            < reinterpret_cast<unsigned long>(r));
    }
};

} // namespace _wrap_


#endif
