/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmJpeg16.cxx
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
#include "itkjpeg/16/jconfig.h"
#include "itkjpeg/16/jpeglib.h"
#include "itkjpeg/16/jinclude.h"
#include "itkjpeg/16/jerror.h"
}

#define gdcm_write_JPEG_file  gdcm_write_JPEG_file16
#define ReadJPEGFile   ReadJPEGFile16
#define SampBuffer SampBuffer16

#include "gdcmJpeg.cxx"

