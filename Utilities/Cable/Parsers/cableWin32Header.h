/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    cableWin32Header.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
#ifndef _cableWin32Header_h
#define _cableWin32Header_h

// Include cxxUtils.h which does some work for us.
#include "cxxUtils.h"

#if defined(_WIN32) || defined(WIN32) /* Win32 version */
#  ifdef BUILD_SHARED_LIBRARIES
#    ifdef PARSERS_LIBRARY
#      define PARSERS_EXPORT __declspec(dllexport)
#    else
#      define PARSERS_EXPORT __declspec(dllimport)
#    endif
#  else
#    define PARSERS_EXPORT
#  endif
#else /* UNIX version */
#  define PARSERS_EXPORT
#endif

// Get the String type from cxxUtils.h.  We don't have to worry about
// a name conflict here.
typedef _cxx_::String String;

#endif
