/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWin32Header.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


Copyright (c) 2000 National Library of Medicine
All rights reserved.

See COPYRIGHT.txt for copyright details.

=========================================================================*/

#ifndef __itkWIN32Header_h
#define __itkWIN32Header_h

// add in the Windows variants
#if defined(_WIN32) || defined(WIN32)
// Uncommenting the following line can cause problems because macros
// like min & max are defined which screws up traits 
// (numeric_limits::min()and such.
//#include <windows.h>

#pragma warning ( disable : 4244 )
#pragma warning ( disable : 4305 )
#pragma warning ( disable : 4309 )

#ifdef ITKDLL
#define ITK_EXPORT __declspec( dllexport ) 
#else
//#define ITK_EXPORT __declspec( dllimport )
#define ITK_EXPORT __declspec( dllexport ) 
#endif

// Now for the UNIX stuff
#else 

#define ITK_EXPORT

#endif

#endif
