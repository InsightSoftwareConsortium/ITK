/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmJpeg8.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$
                                                                                
  Copyright (c) CREATIS (Centre de Recherche et d'Applications en Traitement de
  l'Image). All rights reserved. See Doc/License.txt or
  http://www.creatis.insa-lyon.fr/Public/Gdcm/License.html for details.
                                                                                
     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.
                                                                                
=========================================================================*/
#include "gdcmFileHelper.h"
#include <stdio.h>

extern "C" {
#include "itkjpeg/8/jconfig.h"
#include "itkjpeg/8/jpeglib.h"
#include "itkjpeg/8/jinclude.h"
#include "itkjpeg/8/jerror.h"
}

#define gdcm_write_JPEG_file  gdcm_write_JPEG_file8
#define ReadJPEGFile   ReadJPEGFile8
#define SampBuffer SampBuffer8

#include "gdcmJpeg.cxx"

