/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cxxUtils.h
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
#    ifdef CxxTypes_EXPORTS
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
