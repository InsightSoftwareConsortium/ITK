/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itpack_f2c.h
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#ifndef __itpack_f2c_h
#define __itpack_f2c_h

/**
 * \file itpack_f2c.h
 * \brief Includes all the typedefs required by itpack.cxx and
 * itpack_dsrc2c.c files.
 */




typedef long int integer;
typedef long int logical;
typedef float real;
typedef double doublereal;

typedef long int flag;
typedef long int ftnlen;
typedef long int ftnint;

typedef doublereal E_f;

typedef struct
{   flag cierr;
    ftnint ciunit;
    flag ciend;
    char *cifmt;
    ftnint cirec;
} cilist;


#endif
