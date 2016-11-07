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

#include "itkAtan2ImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"
#include "itkTestingMacros.h"


int itkAtan2ImageFilterTest( int, char* [] )
{

  // Define the dimension of the images
  const unsigned int ImageDimension = 3;

  // Declare the pixel types of the images
  typedef float                PixelType;

  // Declare the types of the images
  typedef itk::Image< PixelType, ImageDimension > InputImageType;
  typedef itk::Image< PixelType, ImageDimension > OutputImageType;

  // Declare appropriate Iterator types for each image
  typedef itk::ImageRegionIteratorWithIndex<
                                  InputImageType >  InputIteratorType;

  typedef itk::ImageRegionIteratorWithIndex<
                                  OutputImageType > OutputIteratorType;

  // Declare the type of the index to access images
  typedef itk::Index< ImageDimension >         IndexType;

  // Declare the type of the size
  typedef itk::Size< ImageDimension >          SizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion< ImageDimension >   RegionType;

  // Create two images
  InputImageType::Pointer sinImage = InputImageType::New();
  InputImageType::Pointer cosImage = InputImageType::New();

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

  // Initialize Sinus Image
  sinImage->SetRegions( region );
  sinImage->Allocate();

  // Initialize Cosinus Image
  cosImage->SetRegions( region );
  cosImage->Allocate();

  // Create one iterator for the Input Image (this is a light object)
  InputIteratorType it1( sinImage, sinImage->GetBufferedRegion() );

  // Initialize the content of Image A
  const double sinValue = std::sin( itk::Math::pi / 6.0 );
  it1.GoToBegin();
  while( !it1.IsAtEnd() )
    {
    it1.Set( sinValue );
    ++it1;
    }

  // Create one iterator for the Input Image (this is a light object)
  InputIteratorType it2( cosImage, cosImage->GetBufferedRegion() );

  // Initialize the content of Image A
  const double cosValue = std::cos( itk::Math::pi / 6.0 );
  it2.GoToBegin();
  while( !it2.IsAtEnd() )
    {
    it2.Set( cosValue );
    ++it2;
    }

  // Declare the type for the Atan filter
  typedef itk::Atan2ImageFilter<
    InputImageType, InputImageType, OutputImageType > FilterType;

  // Create the Filter
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, Atan2ImageFilter,
    BinaryFunctorImageFilter );

  // Set the input images
  filter->SetInput1( sinImage );
  filter->SetInput2( cosImage );

  // Get the filter output
  OutputImageType::Pointer outputImage = filter->GetOutput();

  // Execute the filter
  filter->Update();

  // Create an iterator for going through the image output
  OutputIteratorType ot( outputImage, outputImage->GetRequestedRegion() );

  // Check the content of the result image
  const OutputImageType::PixelType epsilon = 1e-6;
  ot.GoToBegin();

  it1.GoToBegin();
  it2.GoToBegin();

  while( !ot.IsAtEnd() )
    {
    const InputImageType::PixelType  input1 = it1.Get();
    const InputImageType::PixelType  input2 = it2.Get();
    const OutputImageType::PixelType output = ot.Get();
    const OutputImageType::PixelType atan2  = std::atan2( input1, input2 );
    if( !itk::Math::FloatAlmostEqual( atan2, output, 10, epsilon ) )
      {
      std::cerr.precision( static_cast< int >( itk::Math::abs( std::log10( epsilon ) ) ) );
      std::cerr << "Error " << std::endl;
      std::cerr << " std::atan2( " << input1 << ", " << input2 << ") = " << atan2 << std::endl;
      std::cerr << " differs from " << output;
      std::cerr << " by more than " << epsilon << std::endl;
      return EXIT_FAILURE;
      }
    ++ot;
    ++it1;
    }

  return EXIT_SUCCESS;
}
