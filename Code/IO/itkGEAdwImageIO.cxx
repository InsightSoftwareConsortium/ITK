/*=========================================================================
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkGEAdwImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  This software is distributed WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the above copyright notices for more information.

  =========================================================================*/
#include "itkGEAdwImageIO.h"
#include "itkIOCommon.h"
#include <itkkwsys/SystemTools.hxx>

#include "itkExceptionObject.h"
#include "itkByteSwapper.h"
#include "itkGEImageHeader.h"
#include "itkDirectory.h"
#include <iostream>
#include <fstream>
#include <string.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdio.h>
#include <vector>
#include <string>

//From uiig library "The University of Iowa Imaging Group-UIIG"

namespace itk 
{
  // Default constructor
  GEAdwImageIO::GEAdwImageIO()
  {
    //Purposefully left blank
  }

  GEAdwImageIO::~GEAdwImageIO()
  {
    //Purposefully left blank
    
  }

  bool GEAdwImageIO::CanReadFile( const char* FileNameToRead )
  {
    size_t imageSize;
    short matrixX;
    short matrixY;
    int varHdrSize;
    //this->SetFileName(FileNameToRead);
    //
    // Can you open it?
    std::ifstream f(FileNameToRead,std::ios::binary | std::ios::in);
    std::cout << "foo1" << std::endl;
    if(!f.is_open())
      return false;
    //
    // This test basically snoops out the image dimensions, and the
    // length of the variable-length part of the header, and computes
    // the size the file should be and compares it with the actual size.
    // if it's not reading a GEAdw file, chances are overwhelmingly good
    // that this operation will fail somewhere along the line.
std::cout << "foo2" << std::endl;
    if(this->GetShortAt(f,GE_ADW_IM_IMATRIX_X,&matrixX,false) != 0)
      return false;
    
    std::cout << "foo3" << std::endl;
if(this->GetShortAt(f,GE_ADW_IM_IMATRIX_Y,&matrixY,false) != 0)
      return false;

std::cout << "foo4" << std::endl;
    if(this->GetIntAt(f,GE_ADW_VARIABLE_HDR_LENGTH,&varHdrSize,false) != 0)
      return false;

    std::cout << "foo5" << std::endl;
imageSize = varHdrSize + GE_ADW_FIXED_HDR_LENGTH + (matrixX * matrixY * sizeof(short));

std::cout << "foo6" << std::endl;
    if ( imageSize != itkkwsys::SystemTools::FileLength(FileNameToRead) )
      {
  return false;
      }
    return true;
  }

  struct GEImageHeader *GEAdwImageIO::ReadHeader(const char *FileNameToRead)
  {
    char tmpbuf[1024];

    if(!this->CanReadFile(FileNameToRead))
      RAISE_EXCEPTION();

    GEImageHeader *hdr = new struct GEImageHeader;
    if(hdr == 0)
      RAISE_EXCEPTION();
    //
    // Next, can you open it?
    std::ifstream f(FileNameToRead,std::ios::binary | std::ios::in);
    if(!f.is_open())
      RAISE_EXCEPTION();
    
    sprintf(hdr->scanner,"GE-ADW");
    this->GetStringAt(f,GE_ADW_EX_PATID,tmpbuf,12);
    tmpbuf[12] = '\0';
    hdr->patientId[0] = '\0';
    for(char *ptr = strtok(tmpbuf,"-"); ptr != NULL; ptr = strtok(NULL,"-"))
      {
  strcat(hdr->patientId,ptr);
      }

    this->GetStringAt(f,GE_ADW_EX_PATNAME,hdr->name,GE_ADW_EX_PATNAME_LEN);
    hdr->name[GE_ADW_EX_PATNAME_LEN] = '\0';

    this->GetStringAt(f,GE_ADW_EX_HOSPNAME,hdr->hospital,34);
    hdr->hospital[33] = '\0';


    int timeStamp;
    this->GetIntAt(f,GE_ADW_EX_DATETIME,&timeStamp);
    this->statTimeToAscii(&timeStamp,hdr->date);

    this->GetStringAt(f,GE_ADW_SU_PRODID,hdr->scanner,13);
    hdr->scanner[13] = '\0';

    this->GetShortAt(f,GE_ADW_SE_NO,&(hdr->seriesNumber));

    this->GetShortAt(f,GE_ADW_IM_NO,&(hdr->imageNumber));

    this->GetShortAt(f,GE_ADW_IM_CPHASENUM,&(hdr->imagesPerSlice));

    this->GetShortAt(f,GE_ADW_IM_CPHASENUM,&(hdr->turboFactor));

    this->GetFloatAt(f,GE_ADW_IM_SLTHICK,&(hdr->sliceThickness));

    this->GetShortAt(f,GE_ADW_IM_IMATRIX_X,&(hdr->imageXsize));

    this->GetShortAt(f,GE_ADW_IM_IMATRIX_Y,&(hdr->imageYsize));


    hdr->acqXsize = hdr->imageXsize;
    hdr->acqYsize = hdr->imageYsize;
    
    this->GetFloatAt(f,GE_ADW_IM_DFOV,&hdr->xFOV);
    hdr->yFOV = hdr->xFOV;

    this->GetFloatAt(f,GE_ADW_IM_PIXSIZE_X,&hdr->imageXres);

    this->GetFloatAt(f,GE_ADW_IM_PIXSIZE_Y,&hdr->imageYres);

    short tmpShort;
    this->GetShortAt(f,GE_ADW_IM_PLANE,&tmpShort);
    switch (tmpShort)
      {
      case GE_CORONAL:
  hdr->imagePlane = CORONAL;
  break;
      case GE_SAGITTAL:
  hdr->imagePlane = SAGITTAL;
  break;
      case GE_AXIAL:
  hdr->imagePlane = AXIAL;
  break;
      default:
  hdr->imagePlane = CORONAL;
  break;
      }
    this->GetFloatAt(f,GE_ADW_IM_LOC,&(hdr->sliceLocation));

    int tmpInt;
    this->GetIntAt(f,GE_ADW_IM_TR,&tmpInt);
    hdr->TR = (float) tmpInt / 1000.0;

    this->GetIntAt(f,GE_ADW_IM_TI,&tmpInt);
    hdr->TI = (float) tmpInt / 1000.0;

    this->GetIntAt(f,GE_ADW_IM_TE,&tmpInt);
    hdr->TE = (float) tmpInt / 1000.0;

    this->GetShortAt(f, GE_ADW_IM_NUMECHO,&(hdr->numberOfEchoes));

    this->GetShortAt(f, GE_ADW_IM_ECHONUM,&(hdr->echoNumber));

    float tmpFloat;
    this->GetFloatAt(f,GE_ADW_IM_NEX,&tmpFloat);
    
    hdr->NEX = (int) tmpFloat;
    
    this->GetShortAt(f,GE_ADW_IM_MR_FLIP,&hdr->flipAngle);

    this->GetStringAt(f,GE_ADW_IM_PSDNAME, hdr->pulseSequence, 31);
    hdr->pulseSequence[31] = '\0';
    
    this->GetShortAt(f,GE_ADW_IM_SLQUANT,&(hdr->numberOfSlices));

    this->GetIntAt(f,GE_ADW_VARIABLE_HDR_LENGTH,&tmpInt);
    hdr->offset = GE_ADW_FIXED_HDR_LENGTH + tmpInt;

    strncpy (hdr->filename, FileNameToRead, IOCommon::ITK_MAXPATHLEN);
    hdr->filename[IOCommon::ITK_MAXPATHLEN] = '\0';

    return hdr;
  }

} // end namespace itk
