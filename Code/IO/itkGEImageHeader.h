/*=========================================================================

Program:   Insight Segmentation & Registration Toolkit
Module:    itkGEImageHeader.h
Language:  C++
Date:      $Date$
Version:   $Revision$

Copyright (c) 2002 Insight Consortium. All rights reserved.
See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

This software is distributed WITHOUT ANY WARRANTY; without even 
the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef _itkGEImageHeader_H_
#define _itkGEImageHeader_H_

#include "itkIOCommon.h"

#include "idbm_hdr_def.h"
enum GE_PANE_STRUCT {
  GE_AXIAL   =2,
  GE_SAGITTAL=4,
  GE_CORONAL =8
};

struct GEImageHeader
{
  short int seriesNumber;
  short int numberOfEchoes;
  short int echoNumber;
  short int imageNumber;
  float sliceLocation;
  float sliceThickness;
  float sliceGap;
  float TI;
  float TE;
  float TE2;
  float TR;
  short int flipAngle;
  int NEX;
  float xFOV;
  float yFOV;
  short int acqXsize;
  short int acqYsize;
  short int frequencyDir;
  char scanner[16];
  char pulseSequence[128]; //Needs to be at least 65 for seimans vision
  char patientId[32];
  char scanId[32];
  char name[64];
  char date[32];
  short int imageXsize;
  short int imageYsize;
  float imageXres;
  float imageYres;
  short int imagePlane;
  short int numberOfSlices;
  short int offset;
  char filename[itk::IOCommon::ITK_MAXPATHLEN+1];
  char hospital[35];
  short int imagesPerSlice;
  short int turboFactor; //This is only relevant for the geADW image format, but is put here for convenience
};
#endif
