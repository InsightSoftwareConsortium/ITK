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

// include  generic stuff 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fstream.h>
#include <math.h>

// now add in the UNIX / Windows varients
#if defined(_WIN32) || defined(WIN32)
#include <strstrea.h>
#include <windows.h>

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

#include <strstream.h>
#define ITK_EXPORT

#endif

#endif
