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

/**
 * Include Tcl headers.
 */
#include <tcl.h>

#define ObjectTypeIsNULL(o)  ((o)->typePtr == NULL)
#define ObjectTypeIsBoolean(o)  (((o)->typePtr != NULL) \
                                 && (strcmp("boolean", (o)->typePtr->name)==0))
#define ObjectTypeIsInt(o)  (((o)->typePtr != NULL) \
                             && (strcmp("int", (o)->typePtr->name)==0))
#define ObjectTypeIsDouble(o)  (((o)->typePtr != NULL) \
                                && (strcmp("double", (o)->typePtr->name)==0))
#define ObjectTypeIsString(o)  (((o)->typePtr != NULL) \
                                && (strcmp("string", (o)->typePtr->name)==0))
#define ObjectTypeIsCmdName(o)  (((o)->typePtr != NULL) \
                                 && (strcmp("cmdName", (o)->typePtr->name)==0))

#endif
