/*=========================================================================
  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkSiemensVisionImageIO.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  This software is distributed WITHOUT ANY WARRANTY; without even
  the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
  PURPOSE.  See the above copyright notices for more information.

  =========================================================================*/
#include "itkSiemensVisionImageIO.h"
#include "itkIOCommon.h"
#include <itkkwsys/SystemTools.hxx>
#include "itkExceptionObject.h"
#include "itkByteSwapper.h"
#include "itkGEImageHeader.h"
#include <iostream>
#include <fstream>
#include <string.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <vector>
#include <string>

//From uiig library "The University of Iowa Imaging Group-UIIG"

namespace itk 
{
// Default constructor
SiemensVisionImageIO::SiemensVisionImageIO()
{
  //Purposefully left blank
}

SiemensVisionImageIO::~SiemensVisionImageIO()
{
  //Purposefully left blank
    
}

bool SiemensVisionImageIO::CanReadFile( const char* FileNameToRead )
{
  this->SetFileName(FileNameToRead);
  //
  // Can you open it?
  std::ifstream f(FileNameToRead,std::ios::binary | std::ios::in);
  if(!f.is_open())
    return false;
  int matrixX;
  //
  // another lame heuristic, check the actual file size against
  // the image size suggested in header + the header size.
  if(this->GetIntAt(f,HDR_DISPLAY_SIZE,&matrixX,false) != 0)
    return false;

  if((HDR_TOTAL_LENGTH + (matrixX * matrixX * 2)) !=
     (int)itkkwsys::SystemTools::FileLength(FileNameToRead))
    return false;

  return true;
}

struct GEImageHeader *SiemensVisionImageIO::ReadHeader(const char *FileNameToRead)
{
  if(!this->CanReadFile(FileNameToRead))
    RAISE_EXCEPTION();

  int tmpInt;
  double tmpDble;

  // #define DEBUGHEADER
#if defined(DEBUGHEADER)
#define DB(x) std::cerr << #x << " " << x << std::endl
#else
#define DB(x)
#endif

#define GE_PROD_STR    "SIEMENS"
#define TEMPLEN 2048
  char tmpStr[TEMPLEN], tmpStr2[TEMPLEN],
    tmpStr3[TEMPLEN], tmpStr4[TEMPLEN];
  GEImageHeader *hdr = new struct GEImageHeader;
  if(hdr == 0)
    RAISE_EXCEPTION();
#if defined(DEBUGHEADER)
  std::cerr << "----------------------" << FileNameToRead << "----------------------" << std::endl;
#endif
  std::ifstream f(FileNameToRead,std::ios::binary | std::ios::in);
  if(!f.is_open())
    RAISE_EXCEPTION();

  sprintf (hdr->scanner, "GE-ADW");

  strncpy(hdr->filename,FileNameToRead,itk::IOCommon::ITK_MAXPATHLEN);

  // Get VITAL Information from the header
  this->GetStringAt(f,HDR_PAT_ID,hdr->patientId,HDR_PAT_ID_LEN);
  hdr->patientId[HDR_PAT_ID_LEN]= '\0';
  DB(hdr->patientId);
  // fprintf(stderr, "Patient %s\n", hdr->patientId);a

  this->GetStringAt(f, HDR_PAT_NAME,hdr->name, HDR_PAT_NAME_LEN);
  hdr->name[HDR_PAT_NAME_LEN] = '\0';
  DB(hdr->name);

  int year,month,day,hour,minute,second;

  this->GetIntAt(f,HDR_REG_YEAR,&year);
    
  this->GetIntAt(f,HDR_REG_MONTH,&month);
  
  this->GetIntAt(f,HDR_REG_DAY,&day);
  
  this->GetIntAt(f,HDR_REG_HOUR,&hour);
  
  this->GetIntAt(f,HDR_REG_MIN,&minute);
  
  this->GetIntAt(f,HDR_REG_SEC,&second);
    
  sprintf (hdr->date, "%d/%d/%d %d:%d:%d", year, month, day, hour, minute, second);
  DB(hdr->date);

  this->GetStringAt(f, HDR_INSTUTE_NAME,hdr->hospital, HDR_INSTUTE_NAME_LEN);
  hdr->hospital[HDR_INSTUTE_NAME_LEN]='\0';
  DB(hdr->hospital);

  this->GetStringAt(f, HDR_MODEL_NAME,hdr->scanner, HDR_MODEL_NAME_LEN);
  hdr->scanner[HDR_MODEL_NAME_LEN] = '\0';
  DB(hdr->scanner);
  for (unsigned int i=0;i<strlen(hdr->scanner); i++)
    {
    if (hdr->scanner[i] == ' ') hdr->scanner[i] = '-';
    }

  this->GetStringAt(f, TEXT_STUDY_NUM2,tmpStr, TEXT_STUDY_NUM2_LEN);
  tmpStr[TEXT_STUDY_NUM2_LEN] = '\0';
  hdr->seriesNumber = atoi(tmpStr);
  DB(hdr->seriesNumber);

  this->GetStringAt(f, TEXT_IMG_NUMBER,tmpStr, TEXT_IMG_NUMBER_LEN);
  tmpStr[TEXT_IMG_NUMBER_LEN] = '\0';
  hdr->imageNumber = atoi(tmpStr);
  DB(hdr->imageNumber );
    
  this->GetStringAt(f, TEXT_SLICE_THCK,tmpStr, TEXT_SLICE_THCK_LEN);
  tmpStr[TEXT_SLICE_THCK_LEN] = '\0';
  hdr->sliceThickness = atoi(tmpStr);
  DB(hdr->sliceThickness );
  
  this->GetIntAt(f, HDR_DISPLAY_SIZE, &tmpInt, sizeof (int));
  hdr->imageXsize = (int) tmpInt;
  DB(hdr->imageXsize );
  hdr->imageYsize = (int) tmpInt;
  DB(hdr->imageYsize );

  this->GetStringAt(f, TEXT_ACQ_MTRX_FREQ,tmpStr, TEXT_ACQ_MTRX_FREQ_LEN);
  tmpStr[TEXT_ACQ_MTRX_FREQ_LEN] = '\0';
  hdr->acqXsize = atoi(tmpStr);
  DB(hdr->acqXsize );
  
  this->GetStringAt(f, TEXT_ACQ_MTRX_PHASE,tmpStr, TEXT_ACQ_MTRX_PHASE_LEN);
  tmpStr[TEXT_ACQ_MTRX_PHASE_LEN] = '\0';
  hdr->acqYsize = atoi(tmpStr);
  DB(hdr->acqYsize );

  this->GetStringAt(f, TEXT_FOVH,tmpStr, TEXT_FOVH_LEN);
  tmpStr[TEXT_FOVH_LEN] = '\0';
  hdr->xFOV = atof(tmpStr);
  DB(hdr->xFOV );
  
  this->GetStringAt(f, TEXT_FOVV,tmpStr, TEXT_FOVV_LEN);
  tmpStr[TEXT_FOVV_LEN] = '\0';
  hdr->yFOV = atof(tmpStr);
  DB(hdr->yFOV );

  this->GetDoubleAt(f, HDR_PIXELSIZE_ROW,&tmpDble, sizeof (double));
  hdr->imageXres = (float) tmpDble;
  DB(hdr->imageXres );
  
  this->GetDoubleAt(f, HDR_PIXELSIZE_CLMN,&tmpDble, sizeof (double));
  hdr->imageYres = (float) tmpDble;
  DB(hdr->imageYres );
  
  this->GetStringAt(f, TEXT_ANGLE_FLAG1,tmpStr, TEXT_ANGLE_FLAG1_LEN);
  tmpStr[TEXT_ANGLE_FLAG1_LEN] = '\0';
  
  this->GetStringAt(f, TEXT_ANGLE_FLAG2,tmpStr2, TEXT_ANGLE_FLAG2_LEN);
  tmpStr2[TEXT_ANGLE_FLAG2_LEN] = '\0';
  
  this->GetStringAt(f, TEXT_ANGLE_FLAG3,tmpStr3, TEXT_ANGLE_FLAG3_LEN);
  tmpStr3[TEXT_ANGLE_FLAG3_LEN] = '\0';
  
  this->GetStringAt(f, TEXT_ANGLE,tmpStr4, TEXT_ANGLE_LEN);
  tmpStr4[TEXT_ANGLE_LEN] = '\0';
  
  if (strcmp(tmpStr, "Cor") == 0)
    {
    if (fabs(atof(tmpStr4))<= 45.0)
      {
      hdr->imagePlane = CORONAL;
      }
    else
      {
      if (strcmp(tmpStr3, "Sag") == 0)
        {
        hdr->imagePlane = SAGITTAL;
        }
      else
        {
        hdr->imagePlane = AXIAL;
        }
      }
    }
  else if (strcmp(tmpStr, "Sag") == 0)
    {
    if (fabs(atof(tmpStr4))<= 45.0)
      {
      hdr->imagePlane = SAGITTAL;
      }
    else
      {
      if (strcmp(tmpStr3, "Cor") == 0)
        {
        hdr->imagePlane = CORONAL;
        }
      else
        {
        hdr->imagePlane = AXIAL;
        }
      }
    }
  else
    {
    if (fabs(atof(tmpStr4))<= 45.0)
      {
      hdr->imagePlane = AXIAL;
      }
    else
      {
      if (strcmp(tmpStr3, "Cor") == 0)
        {
        hdr->imagePlane = CORONAL;
        }
      else
        {
        hdr->imagePlane = SAGITTAL;
        }
      }
    }
  
  
  /* fprintf(stderr, "Plane %d\n", hdr->imagePlane); */
  this->GetStringAt(f, TEXT_SLICE_POS,tmpStr, TEXT_SLICE_POS_LEN);
  tmpStr[TEXT_SLICE_POS_LEN] = '\0';
  hdr->sliceLocation = atof(tmpStr);
  DB(hdr->sliceLocation );

  /* fprintf(stderr, "Slice Location %f\n", hdr->sliceLocation); */
  this->GetDoubleAt(f, HDR_TR,&tmpDble, sizeof (double));
  hdr->TR = (float) tmpDble / 1000.0;
  DB(hdr->TR );

  this->GetDoubleAt(f,  HDR_TE+8,&tmpDble, sizeof (double));
  hdr->TI = (float) tmpDble / 1000.0;
  DB(hdr->TI );

  this->GetDoubleAt(f,  HDR_TE,&tmpDble, sizeof (double));
  hdr->TE = (float) tmpDble / 1000.0;
  DB(hdr->TE );

  this->GetStringAt(f, TEXT_ECHO_NUM,tmpStr, TEXT_ECHO_NUM_LEN);
  tmpStr[TEXT_ECHO_NUM_LEN] = '\0';
  hdr->echoNumber = (int) atoi(tmpStr);
  DB(hdr->echoNumber );

  this->GetDoubleAt(f,  HDR_FLIP_ANGLE,&tmpDble, sizeof (double));
  hdr->flipAngle = (int) tmpDble;
  DB(hdr->flipAngle );
  
  this->GetStringAt(f, HDR_SEQPROG_NAME,hdr->pulseSequence, HDR_SEQPROG_NAME_LEN);
  hdr->pulseSequence[HDR_SEQPROG_NAME_LEN] = '\0';

  hdr->offset = HDR_TOTAL_LENGTH;
  return hdr;
}

} // end namespace itk
