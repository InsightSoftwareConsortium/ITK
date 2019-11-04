/*=========================================================================

  Program: GDCM (Grassroots DICOM). A DICOM library

  Copyright (c) 2006-2011 Mathieu Malaterre
  All rights reserved.
  See Copyright.txt or http://gdcm.sourceforge.net/Copyright.html for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "gdcmJPEG12Codec.h"

#include "gdcm_ljpeg12.h"

#include <setjmp.h>

#define JPEGBITSCodec JPEG12Codec
#define my_error_mgr my_error_mgr_12BIT
#define JPEGInternals JPEGInternals_12BIT
#define my_source_mgr my_source_mgr_12BIT
#define my_destination_mgr my_destination_mgr_12BIT
#include "gdcmJPEGBITSCodec.hxx"
