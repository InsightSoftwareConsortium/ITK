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

  const unsigned int               Dimension = 3;
  typedef itk::Index<Dimension>    PixelType;

  typedef itk::Image< PixelType, 
                      Dimension >   ImageType;

  typedef itk::RegionOfInterestImageFilter< 
                                      ImageType,
                                      ImageType  > FilterType;


  typedef ImageType::RegionType   RegionType;
  typedef ImageType::SizeType     SizeType;
  typedef ImageType::IndexType    IndexType;


  typedef itk::ImageRegionIterator< 
                           ImageType > IteratorType;

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

  // Fill the image pixels with their own index.
  IteratorType intr( image, region );
  intr.GoToBegin();
  while( !intr.IsAtEnd() )
    {
    intr.Set( intr.GetIndex() );
    ++intr;
    }


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



  IteratorType ot( filter->GetOutput(),
                   filter->GetOutput()->GetLargestPossibleRegion() );

  IteratorType it( image, regionOfInterest );

  it.GoToBegin();
  ot.GoToBegin();

  bool passed = true;
  while( !it.IsAtEnd() )
    {
    IndexType inIndex  = it.Get(); 
    IndexType outIndex = ot.Get(); 
    if( inIndex[0] != outIndex[0]  ||
        inIndex[1] != outIndex[1]  ||
        inIndex[2] != outIndex[2]    )
      {
      std::cerr << "Test failed at pixel " << inIndex << std::endl;
      std::cerr << "pixel value is       " << outIndex << std::endl;
      passed = false;
      break;
      }

    ++it;
    ++ot;
    }

  if( !passed ) 
    {
    return EXIT_FAILURE;
    }

  std::cout << "Test PASSED !" << std::endl;
  return EXIT_SUCCESS;

}




