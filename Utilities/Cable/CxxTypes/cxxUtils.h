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

#  ifdef BUILD_SHARED_LIBRARIES
#    ifdef _cxx_LIBRARY
#      define _cxx_EXPORT __declspec(dllexport)
#    else
#      define _cxx_EXPORT __declspec(dllimport)
#    endif
#  else
#    define _cxx_EXPORT
#  endif

#  ifndef __CYGWIN__
#    define _cxx_char_traits char_traits
#  else
#    define _cxx_STATIC_ALLOCATOR_METHODS
#    define _cxx_char_traits string_char_traits
#  endif

#  define _cxxUtils_DllAllocator_include
#include "cxxDllAllocator.h"
#  undef _cxxUtils_DllAllocator_include

namespace _cxx_
{
/**
 * Define the type "String" to be just like the STL "string", but with our
 * DLL-boundary-safe allocator for the Win32 version.
 */
typedef std::basic_string<char, std::_cxx_char_traits<char>, DllAllocator<char> >  String;
} // namespace _cxx_

#else /* UNIX version */

#  define _cxx_EXPORT

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
