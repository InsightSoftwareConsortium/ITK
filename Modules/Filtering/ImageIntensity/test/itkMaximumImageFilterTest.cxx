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

#include "itkMaximumImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkTestingMacros.h"


int itkMaximumImageFilterTest( int, char*[] )
{

  // Define the dimension of the images
  const unsigned int Dimension = 3;

  typedef unsigned char PixelType;

  // Declare the types of the images
  typedef itk::Image< PixelType, Dimension >  ImageType;

  // Declare the type of the index to access images
  typedef itk::Index< Dimension >             IndexType;

  // Declare the type of the size
  typedef itk::Size< Dimension >              SizeType;

  // Declare the type of the region
  typedef itk::ImageRegion< Dimension >       RegionType;

  // Declare the type for the filter
  typedef itk::MaximumImageFilter< ImageType, ImageType,
    ImageType > MaximumImageFilterType;

  // Create two images
  ImageType::Pointer inputImageA = ImageType::New();
  ImageType::Pointer inputImageB = ImageType::New();

  // Define their size, and start index
  SizeType size;
  size[0] = 2;
  size[1] = 2;
  size[2] = 2;

  IndexType start;
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize Image A
  inputImageA->SetLargestPossibleRegion( region );
  inputImageA->SetBufferedRegion( region );
  inputImageA->SetRequestedRegion( region );
  inputImageA->Allocate();

  // Initialize Image B
  inputImageB->SetLargestPossibleRegion( region );
  inputImageB->SetBufferedRegion( region );
  inputImageB->SetRequestedRegion( region );
  inputImageB->Allocate();

  // Define the pixel values for each image
  PixelType largePixelValue = 3;
  PixelType smallPixelValue = 2;

  // Declare Iterator types apropriated for each image
  typedef itk::ImageRegionIteratorWithIndex< ImageType > IteratorType;

  // Create one iterator for Image A (this is a light object)
  IteratorType it1( inputImageA, inputImageA->GetBufferedRegion() );

  // Initialize the content of Image A
  while( !it1.IsAtEnd() )
  {
    it1.Set( smallPixelValue );
    ++it1;
  }

  // Create one iterator for Image B (this is a light object)
  IteratorType it2( inputImageB, inputImageB->GetBufferedRegion() );

  // Initialize the content of Image B
  while( !it2.IsAtEnd() )
  {
    it2.Set( largePixelValue );
    ++it2;
  }

  // Create the filter
  MaximumImageFilterType::Pointer maximumImageFilter = MaximumImageFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( maximumImageFilter, MaximumImageFilter,
    BinaryFunctorImageFilter);

  // Connect the input images
  maximumImageFilter->SetInput1( inputImageA );
  maximumImageFilter->SetInput2( inputImageB );

  // Get the Smart Pointer to the filter output
  ImageType::Pointer outputImage = maximumImageFilter->GetOutput();

  maximumImageFilter->SetFunctor( maximumImageFilter->GetFunctor() );

  // Execute the filter
  maximumImageFilter->Update();

  // Test some pixels in the result image
  // Note that we are not comparing the entirety of the filter output in order
  // to keep compile time as small as possible

  ImageType::IndexType pixelIndex = {{0, 1, 1}};

  TEST_EXPECT_EQUAL( outputImage->GetPixel( start ), largePixelValue );
  TEST_EXPECT_EQUAL( outputImage->GetPixel( pixelIndex ), largePixelValue );

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;

}
