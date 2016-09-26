/*=========================================================================
 *
 *  Copyright Insight Software Consortium
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <iostream>
#include "itkRegionOfInterestImageFilter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int itkRegionOfInterestImageFilterTest( int, char* [] )
{

  const unsigned int              Dimension = 3;
  typedef itk::Index< Dimension > PixelType;

  typedef itk::Image< PixelType, Dimension > ImageType;

  typedef itk::RegionOfInterestImageFilter<
                                      ImageType,
                                      ImageType > FilterType;


  typedef ImageType::RegionType    RegionType;
  typedef ImageType::SizeType      SizeType;
  typedef ImageType::IndexType     IndexType;
  typedef ImageType::DirectionType DirectionType;

  typedef itk::ImageRegionIterator< ImageType > IteratorType;

  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, RegionOfInterestImageFilter, ImageToImageFilter );

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

  DirectionType directions;
  directions.SetIdentity();
  directions[0][0] = 0.0;
  directions[1][0] = 1.0;
  directions[2][0] = 0.0;
  directions[0][1] = 1.0;
  directions[1][1] = 0.0;
  directions[2][1] = 0.0;
  image->SetDirection( directions );

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

  itk::SimpleFilterWatcher watcher(filter);

  filter->SetRegionOfInterest( regionOfInterest );
  TEST_SET_GET_VALUE( regionOfInterest, filter->GetRegionOfInterest() );

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
    if( inIndex[0] != outIndex[0] ||
        inIndex[1] != outIndex[1] ||
        inIndex[2] != outIndex[2] )
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
