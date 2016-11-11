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
#include "itkTestingMacros.h"

int itkCropImageFilter3DTest( int, char* [] )
{

  // Define the dimension of the images
  const unsigned int ImageDimension = 3;

  // Declare the pixel types of the images
  typedef unsigned short                          PixelType;

  // Declare the types of the images
  typedef itk::Image< PixelType, ImageDimension > ImageType;

  ImageType::RegionType region;
  const unsigned int dimSize(8);
  ImageType::RegionType::SizeType size = {{dimSize, dimSize, dimSize}};
  ImageType::RegionType::IndexType index = {{0, 0, 0}};
  region.SetSize(size);
  region.SetIndex(index);

  ImageType::Pointer image = ImageType::New();

  image->SetRegions(region);
  image->Allocate();

  itk::ImageRegionIterator< ImageType > it( image, region );
  for( unsigned short i = 0; !it.IsAtEnd(); ++it, ++i )
    {
    it.Set( i );
    }

  itk::CropImageFilter< ImageType, ImageType> ::Pointer cropFilter =
    itk::CropImageFilter< ImageType, ImageType >::New();

  EXERCISE_BASIC_OBJECT_METHODS( cropFilter, CropImageFilter,
    ExtractImageFilter );

  cropFilter->SetInput( image );

  // Set the filter properties
  ImageType::SizeType extractSize = {{1, 1, 1}};

  cropFilter->SetBoundaryCropSize( extractSize );

  cropFilter->SetUpperBoundaryCropSize( extractSize );
  TEST_SET_GET_VALUE( extractSize, cropFilter->GetUpperBoundaryCropSize() );

  cropFilter->SetLowerBoundaryCropSize( extractSize );
  TEST_SET_GET_VALUE( extractSize, cropFilter->GetLowerBoundaryCropSize() );

  TRY_EXPECT_NO_EXCEPTION( cropFilter->Update() );

  ImageType::Pointer croppedImage = cropFilter->GetOutput();

  // check size of cropped image
  ImageType::RegionType::SizeType croppedSize =
    croppedImage->GetLargestPossibleRegion().GetSize();
  for( unsigned i = 0; i < ImageType::RegionType::SizeType::GetSizeDimension();
    i++ )
    {
    if( croppedSize[i] != dimSize - 2 )
      {
      std::cerr << "Unexpected cropped Image size[" << i
                << "] = " << croppedSize[i] << ". Expected "
                << dimSize << std::endl;
      return EXIT_FAILURE;
      }
    }
  ImageType::RegionType subRegion;
  ImageType::RegionType::SizeType subSize = {{dimSize-2, dimSize-2, dimSize-2}};
  ImageType::RegionType::IndexType subIndex = {{1, 1, 1}};
  subRegion.SetSize( subSize );
  subRegion.SetIndex( subIndex );

  itk::ImageRegionIterator< ImageType > it1( image, subRegion );
  itk::ImageRegionIterator< ImageType > it2( croppedImage,
    croppedImage->GetLargestPossibleRegion() );
  for(; !it1.IsAtEnd() && !it2.IsAtEnd(); ++it1, ++it2)
    {
    if( it1.Get() != it2.Get() )
      {
      std::cerr << "Expected Pixel " << it1.Get()
                << " saw " << it2.Get() << " instead."
                << std::endl;
      return EXIT_FAILURE;
      }
    }

  return EXIT_SUCCESS;
}
