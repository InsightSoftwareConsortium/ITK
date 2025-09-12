/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef GDCM_LJPEG12_H
#define GDCM_LJPEG12_H

/* Use the ljpeg library configured for gdcm.  */
#include "gdcmTypes.h"

#ifdef GDCM_USE_SYSTEM_LJPEG
extern "C" {
#include "itk_jpeg.h"
#ifndef SIZEOF
#define SIZEOF(X) sizeof(X)
#endif
}
#else
extern "C" {
#include "gdcmjpeg/12/jinclude.h"
#include "gdcmjpeg/12/jpeglib.h"
#include "gdcmjpeg/12/jerror.h"
}
#endif

#endif
