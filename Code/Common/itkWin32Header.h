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
#include <windows.h>

//
// Disable some common warnings in MS VC++
//

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
#pragma warning ( disable : 4251 )

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

