/*=========================================================================
                                                                                
  Program:   gdcm
  Module:    gdcmJpeg12.cxx
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
#include "itkjpeg/12/jconfig.h"
#include "itkjpeg/12/jpeglib.h"
#include "itkjpeg/12/jinclude.h"
#include "itkjpeg/12/jerror.h"
}

#define gdcm_write_JPEG_file  gdcm_write_JPEG_file12
#define ReadJPEGFile   ReadJPEGFile12
#define SampBuffer SampBuffer12

#include "gdcmJpeg.cxx"

