/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkIntTypes.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkIntTypes_h
#define __itkIntTypes_h

#ifdef __cplusplus
extern "C" {
#endif

/** Convenient and more descriptive integer types. */
typedef char      ITK_INT8;
typedef int       ITK_INT32;

#ifndef WIN32
typedef long long   ITK_INT64;
#endif

#ifdef WIN32
typedef long      ITK_INT64;
#endif

typedef unsigned char ITK_UINT8;
typedef unsigned short  ITK_UINT16;
typedef unsigned    ITK_UINT32;

#ifndef WIN32
typedef unsigned long long  ITK_UINT64;
#endif

#ifdef WIN32
typedef unsigned long ITK_UINT64;
#endif

typedef int       ITK_INTPTR;
typedef unsigned    ITK_UINTPTR;

#ifdef __cplusplus
}
#endif

#endif  /* __itkIntTypes_h */

