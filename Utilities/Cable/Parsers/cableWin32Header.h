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

// add in the Windows variants
#if (defined(_WIN32) || defined(WIN32)) && !defined(__CYGWIN__)

// for-loop scoping hack
#define for if(false) {} else for

//
// Disable some common warnings in MS VC++
//

// 'identifier' : identifier was truncated to 'number' characters in the
// debug information
#pragma warning ( disable : 4786 )

#endif

#endif
