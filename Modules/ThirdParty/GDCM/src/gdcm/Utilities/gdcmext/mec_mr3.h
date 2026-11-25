/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#ifndef MEC_MR3_H
#define MEC_MR3_H

#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

void *mec_mr3_memcpy(void *dest, const void *src, size_t n);

#ifdef __cplusplus
} /* end extern "C" */
#endif

#endif  // MEC_MR3_H
