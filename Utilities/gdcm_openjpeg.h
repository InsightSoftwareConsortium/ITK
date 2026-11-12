/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCM_OPENJPEG_H
#define GDCM_OPENJPEG_H

/* Use the openjpeg library configured for gdcm.  */
#include "gdcmTypes.h"
#ifdef GDCM_USE_SYSTEM_OPENJPEG
/* BEGIN ITK */
/* Route through ITK's openjpeg wrapper. For the vendored build this
   also applies the itkopenjpeg_* symbol mangle via openjpeg_mangle.h.
   opj_includes.h is intentionally NOT included to avoid its
   GCC poison pragmas on malloc/free.
   The visibility push ensures openjpeg symbols keep default visibility
   even when the enclosing ITK module is compiled with -fvisibility=hidden. */
#include "itk_openjpeg.h"
/* END ITK */
#else
#include "gdcmopenjpeg/src/lib/openjp2/openjpeg.h"
#endif

#endif
