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
#include "GeAdvWin.h"
#include "itkIOCommon.h"

#include "itkExceptionObject.h"
#include "itkByteSwapper.h"
#include "itkGEImageHeader.h"
#include "idbm_hdr_def.h"
#include "itkMvtSunf.h"
#include "itkDirectory.h"
#include <cstdio>
#include <iostream>
#include <fstream>
#include <string.h>
#include <limits.h>
#include <stdlib.h>
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
    this->SetFileName(FileNameToRead);
    //
    // Can you open it?
    std::ifstream f(FileNameToRead,std::ifstream::binary);
    if(!f.is_open())
      return false;
    //
    // This test basically snoops out the image dimensions, and the
    // length of the variable-length part of the header, and computes
    // the size the file should be and compares it with the actual size.
    // if it's not reading a GEAdw file, chances are overwhelmingly good
    // that this operation will fail somewhere along the line.
    if(this->GetShortAt(f,IM_IMATRIX_X,&matrixX,false) != 0)
      return false;
    
    if(this->GetShortAt(f,IM_IMATRIX_Y,&matrixY,false) != 0)
      return false;

    if(this->GetIntAt(f,VARIABLE_HDR_LENGTH,&varHdrSize,false) != 0)
      return false;

    imageSize = varHdrSize + FIXED_HDR_LENGTH + (matrixX * matrixY * sizeof(short));

    if ( imageSize != itk::IOCommon::FileLength(FileNameToRead) )
      {
  return false;
      }
    return true;
  }

  struct GEImageHeader *GEAdwImageIO::ReadHeader(const char *FileNameToRead)
  {
    char tmpbuf[1024];
    GEImageHeader *hdr = new struct GEImageHeader;
    if(hdr == 0)
      RAISE_EXCEPTION();
    //
    // Next, can you open it?
    std::ifstream f(FileNameToRead,std::ifstream::binary);
    if(!f.is_open())
      RAISE_EXCEPTION();
    
    sprintf(hdr->scanner,"GE-ADW");
    this->GetStringAt(f,EX_PATID,tmpbuf,12);
    tmpbuf[12] = '\0';
    hdr->patientId[0] = '\0';
    for(char *ptr = strtok(tmpbuf,"-"); ptr != NULL; ptr = strtok(NULL,"-"))
      {
  strcat(hdr->patientId,ptr);
      }

    this->GetStringAt(f,EX_PATNAME,hdr->name,EX_PATNAME_LEN);
    hdr->name[EX_PATNAME_LEN] = '\0';

    this->GetStringAt(f,EX_HOSPNAME,hdr->hospital,34);
    hdr->hospital[33] = '\0';


    int timeStamp;
    this->GetIntAt(f,EX_DATETIME,&timeStamp);
    this->statTimeToAscii(&timeStamp,hdr->date);

    this->GetStringAt(f,SU_PRODID,hdr->scanner,13);
    hdr->scanner[13] = '\0';

    this->GetShortAt(f,SE_NO,&(hdr->seriesNumber));

    this->GetShortAt(f,IM_NO,&(hdr->imageNumber));

    this->GetShortAt(f,IM_CPHASENUM,&(hdr->imagesPerSlice));

    this->GetShortAt(f,IM_CPHASENUM,&(hdr->turboFactor));

    this->GetFloatAt(f,IM_SLTHICK,&(hdr->sliceThickness));

    this->GetShortAt(f,IM_IMATRIX_X,&(hdr->imageXsize));

    this->GetShortAt(f,IM_IMATRIX_Y,&(hdr->imageYsize));


    hdr->acqXsize = hdr->imageXsize;
    hdr->acqYsize = hdr->imageYsize;
    
    this->GetFloatAt(f,IM_DFOV,&hdr->xFOV);
    hdr->yFOV = hdr->xFOV;

    this->GetFloatAt(f,IM_PIXSIZE_X,&hdr->imageXres);

    this->GetFloatAt(f,IM_PIXSIZE_Y,&hdr->imageYres);

    short tmpShort;
    this->GetShortAt(f,IM_PLANE,&tmpShort);
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
    this->GetFloatAt(f,IM_LOC,&(hdr->sliceLocation));

    int tmpInt;
    this->GetIntAt(f,IM_TR,&tmpInt);
    hdr->TR = (float) tmpInt / 1000.0;

    this->GetIntAt(f,IM_TI,&tmpInt);
    hdr->TI = (float) tmpInt / 1000.0;

    this->GetIntAt(f,IM_TE,&tmpInt);
    hdr->TE = (float) tmpInt / 1000.0;

    this->GetShortAt(f, IM_NUMECHO,&(hdr->numberOfEchoes));

    this->GetShortAt(f, IM_ECHONUM,&(hdr->echoNumber));

    float tmpFloat;
    this->GetFloatAt(f,IM_NEX,&tmpFloat);
    
    hdr->NEX = (int) tmpFloat;
    
    this->GetShortAt(f,IM_MR_FLIP,&hdr->flipAngle);

    this->GetStringAt(f,IM_PSDNAME, hdr->pulseSequence, 31);
    hdr->pulseSequence[31] = '\0';
    
    this->GetShortAt(f,IM_SLQUANT,&(hdr->numberOfSlices));

    this->GetIntAt(f,VARIABLE_HDR_LENGTH,&tmpInt);
    hdr->offset = FIXED_HDR_LENGTH + tmpInt;

    strncpy (hdr->filename, FileNameToRead, MAXPATHLEN);
    hdr->filename[MAXPATHLEN] = '\0';

    return hdr;
  }

} // end namespace itk
