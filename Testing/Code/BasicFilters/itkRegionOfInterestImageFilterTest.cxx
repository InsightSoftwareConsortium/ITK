/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkRegionOfInterestImageFilterTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include <iostream>
#include "itkImage.h"
#include "itkImageRegionIterator.h"
#include "itkRegionOfInterestImageFilter.h"

int itkRegionOfInterestImageFilterTest(int, char* [] )
{

  typedef unsigned short      PixelType;
  const unsigned int          Dimension = 3;

  typedef itk::Image< PixelType, Dimension > ImageType;

  typedef itk::RegionOfInterestImageFilter< 
                                      ImageType,
                                      ImageType  > FilterType;


  typedef ImageType::RegionType   RegionType;
  typedef ImageType::SizeType     SizeType;
  typedef ImageType::IndexType    IndexType;


  FilterType::Pointer filter = FilterType::New();
 

  ImageType::Pointer image = ImageType::New();

  IndexType start;
  start.Fill( 0 );
 
  SizeType  size;
  size[0] = 40;
  size[1] = 40;
  size[2] = 40;

  RegionType region;
  region.SetIndex( start );
  region.SetSize(  size  );

  image->SetRegions( region );
  image->Allocate();

  image->FillBuffer( 217 );

  filter->SetInput( image );

  SizeType roiSize;
  roiSize[0] = 20;
  roiSize[1] = 20;
  roiSize[2] = 20;

  IndexType roiStart;
  roiStart[0] = 9;
  roiStart[1] = 9;
  roiStart[2] = 9;

  RegionType regionOfInterest;
  regionOfInterest.SetIndex( roiStart );
  regionOfInterest.SetSize(  roiSize  );

  filter->SetRegionOfInterest( regionOfInterest );
  

  filter->Update();

  return EXIT_SUCCESS;
}




