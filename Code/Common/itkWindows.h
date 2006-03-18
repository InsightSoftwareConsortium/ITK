/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkWindows.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
/** This file is used to create the smallest windows.h possible.
 * Also it removes a few annoying #define's in windows.h. */
#ifndef __itkWindows_h
#define __itkWindows_h
#ifndef NOMINMAX
#define NOMINMAX
#endif
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <winbase.h>
#endif
