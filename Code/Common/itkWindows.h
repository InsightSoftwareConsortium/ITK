/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWindows.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$


  Copyright (c) 2000 National Library of Medicine
  All rights reserved.

  See COPYRIGHT.txt for copyright details.

=========================================================================*/
// This file is used to create the smallest windows.h possible.
// Also it removes a few annoying #define's in windows.h
#ifndef __itkWindows_h
#define __itkWindows_h
#define NOMINMAX
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <winbase.h>
#undef GetClassName
#endif

