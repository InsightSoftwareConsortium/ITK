/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWin32Header.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkWIN32Header_h
#define __itkWIN32Header_h

// add in the Windows variants

#if defined(__CYGWIN__)
#ifndef WIN32
#define WIN32 1
#endif
#ifndef _WIN32
#define _WIN32 1
#endif
#endif

/** Disable some common warnings in MS VC++ */
#if defined(_MSC_VER)

// 'conversion' conversion from 'type1' to 'type2', possible loss of data
#pragma warning ( disable : 4244 )

// 'identifier' : truncation from 'type1' to 'type2'
#pragma warning ( disable : 4305 )

// 'conversion' : truncation of constant value
#pragma warning ( disable : 4309 )

// 'identifier' : identifier was truncated to 'number' characters in the
// debug information
#pragma warning ( disable : 4786 )

// 'type' : forcing value to bool 'true' or 'false' (performance warning)
#pragma warning ( disable : 4800 )

// 'identifier' : class 'type' needs to have dll-interface to be used by
// clients of class 'type2'
// #pragma warning ( disable : 4251 )

// C++ exception specification ignored except to indicate...
#pragma warning ( disable : 4267 )

// C++ exception specification ignored except to indicate a 
// function is not __declspec(nothrow)
#pragma warning ( disable : 4290 )


// typename keyword in default template arguments is not accepted by
// MSVC.  This macro should only be used in such places.
#if !defined(CABLE_CONFIGURATION)
#define ITK_TYPENAME
#else
#define ITK_TYPENAME typename
#endif
#else
#define ITK_TYPENAME typename
#endif

#if defined(_WIN32) || defined(WIN32)
# ifndef ITKSTATIC
#  ifdef ITKDLL
#   define ITK_EXPORT __declspec( dllexport ) 
#  else
#   define ITK_EXPORT 
#  endif
# else
#  define ITK_EXPORT
# endif  // ITKSTATIC
#else
// Now for the UNIX stuff

#define ITK_EXPORT

#endif

#endif
