/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxUtils.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _cxxUtils_h
#define _cxxUtils_h

// Disable some Visual C++ warnings.
#ifdef _MSC_VER
#pragma warning ( disable : 4786 )
#pragma warning ( disable : 4251 )
#endif

// Disable some Intel C++ Compiler warnings.
#ifdef __ICL
#pragma warning ( disable : 985 )
#endif

// Visual C++ for-loop scoping hack.
#ifdef _MSC_VER
#ifndef for
#define for if(false) {} else for
#endif
#endif

#include <string>

/**
 * Setup a few utilities used by almost all source files.  There are some
 * differences between UNIX and Win32 platforms that are addressed here.
 */
#if defined(_WIN32) || defined(WIN32) /* Win32 version */

#define _cxx_IMPORT __declspec( dllimport )
#define _cxx_EXPORT __declspec( dllexport )

#include "cxxDllAllocator.h"

namespace _cxx_
{
/**
 * Define the type "String" to be just like the STL "string", but with our
 * DLL-boundary-safe allocator for the Win32 version.
 */
typedef std::basic_string<char, std::char_traits<char>, DllAllocator<char> >  String;
} // namespace _cxx_

#else /* UNIX version */

#define _cxx_IMPORT
#define _cxx_EXPORT

namespace _cxx_
{
/**
 * Define the type "String" to be just like the STL "string".  In UNIX,
 * there are no problems with this in shared libraries.
 */
typedef std::string  String;
} // namespace _cxx_

#endif

#endif
