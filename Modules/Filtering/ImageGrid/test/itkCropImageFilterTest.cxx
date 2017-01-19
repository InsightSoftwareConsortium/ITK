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
#include "itkCropImageFilter.h"
#include "itkFilterWatcher.h"
#include "itkTestingMacros.h"

int itkCropImageFilterTest( int, char* [] )
{

  // Define the dimension of the images
  const unsigned int ImageDimension = 2;

  // Declare the pixel types of the images
  typedef short                                   PixelType;

  // Declare the types of the images
  typedef itk::Image< PixelType, ImageDimension > ImageType;

  ImageType::Pointer inputImage = ImageType::New();

  // Fill in the image
  ImageType::IndexType  index = {{0, 0}};
  ImageType::SizeType   size = {{8, 12}};
  ImageType::RegionType region;

  region.SetSize( size );
  region.SetIndex( index );
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->Allocate();

  itk::ImageRegionIterator< ImageType > iterator( inputImage, region );

  short i = 0;
  for(; !iterator.IsAtEnd(); ++iterator, ++i)
    {
      iterator.Set( i );
    }

  // Create the filter
  itk::CropImageFilter< ImageType, ImageType >::Pointer cropFilter =
    itk::CropImageFilter< ImageType, ImageType >::New();

  EXERCISE_BASIC_OBJECT_METHODS( cropFilter, CropImageFilter,
    ExtractImageFilter );

  FilterWatcher watcher( cropFilter );

  cropFilter->SetInput( inputImage );

  ImageType::RegionType requestedRegion;

  ImageType::SizeType extractSize = {{8, 12}};
  extractSize[0] = 1;
  extractSize[1] = 1;

  cropFilter->SetBoundaryCropSize( extractSize );

  cropFilter->SetUpperBoundaryCropSize( extractSize );
  TEST_SET_GET_VALUE( extractSize, cropFilter->GetUpperBoundaryCropSize() );

  cropFilter->SetLowerBoundaryCropSize( extractSize );
  TEST_SET_GET_VALUE( extractSize, cropFilter->GetLowerBoundaryCropSize() );

  cropFilter->UpdateLargestPossibleRegion();

  requestedRegion = cropFilter->GetOutput()->GetRequestedRegion();

  if( cropFilter->GetOutput()->GetLargestPossibleRegion().GetSize()[0] != 6
      || cropFilter->GetOutput()->GetLargestPossibleRegion().GetSize()[1] != 10 )
    {
      return EXIT_FAILURE;
    }

  if( cropFilter->GetOutput()->GetLargestPossibleRegion().GetIndex()[0] != 1
      || cropFilter->GetOutput()->GetLargestPossibleRegion().GetIndex()[1] != 1 )
    {
      return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
