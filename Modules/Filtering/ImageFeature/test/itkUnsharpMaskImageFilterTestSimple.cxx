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

#include "itkUnsharpMaskImageFilter.h"
#include "itkFilterWatcher.h"
#include "itkTestingMacros.h"

int itkUnsharpMaskImageFilterTestSimple( int, char* [] )
{
  // Define the dimension of the images
  const unsigned int Dimension = 2;

  // Define the pixel types of the images
  typedef float                PixelType;

  // Declare the types of the images
  typedef itk::Image< PixelType, Dimension > InputImageType;

  // Declare the type of the index to access images
  typedef itk::Index< Dimension > IndexType;

  // Declare the type of the size
  typedef itk::Size< Dimension > SizeType;

  // Declare the type of the Region
  typedef itk::ImageRegion< Dimension > RegionType;

  // Create the input image
  InputImageType::Pointer inputImage = InputImageType::New();

  // Define its size, and start index
  SizeType size;
  size[0] = 20;
  size[1] = 4;

  IndexType start;
  start.Fill( 0 );

  RegionType region;
  region.SetIndex( start );
  region.SetSize( size );

  // Initialize the input image
  inputImage->SetLargestPossibleRegion( region );
  inputImage->SetBufferedRegion( region );
  inputImage->SetRequestedRegion( region );
  inputImage->Allocate();

  // Declare an Iterator type for the input image
  typedef itk::ImageRegionIteratorWithIndex< InputImageType >
    InputImageIteratorType;

  // Create one iterator for the input Image (this is a light object)
  InputImageIteratorType it( inputImage, inputImage->GetRequestedRegion() );

  // Initialize the contents of the input image
  while( !it.IsAtEnd() )
    {
      if( it.GetIndex()[0] > itk::IndexValueType(size[0] / 2) )
        {
        it.Set( 1.0 );
        }
      else
        {
        it.Set( 0.0 );
        }

    ++it;
    }

  // Declare the type for the itk::UnsharpMaskImageFilter
  typedef itk::UnsharpMaskImageFilter< InputImageType >
    UnsharpMaskImageFilterFilterType;

  typedef UnsharpMaskImageFilterFilterType::OutputImageType
    GradientImageType;

  // Create the filter
  UnsharpMaskImageFilterFilterType::Pointer filter =
    UnsharpMaskImageFilterFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS(filter, UnsharpMaskImageFilter, ImageToImageFilter);

  FilterWatcher watchit( filter );

  // Connect the input images
  filter->SetInput( inputImage );


  UnsharpMaskImageFilterFilterType::InternalPrecisionType threshold = -0.1;
  filter->SetThreshold( threshold );

  TRY_EXPECT_EXCEPTION( filter->Update() );


  // Set the filter properties
  UnsharpMaskImageFilterFilterType::SigmaArrayType::ValueType sigma = 2.5;
  filter->SetSigma( sigma );

  UnsharpMaskImageFilterFilterType::SigmaArrayType sigmas =
    filter->GetSigmas();

  double tolerance = 10e-6;
  for( unsigned int i = 0; i < sigmas.Size(); ++i )
    {
    UnsharpMaskImageFilterFilterType::SigmaArrayType::ValueType sigma2 =
      sigmas[i];
    if( !itk::Math::FloatAlmostEqual( sigma, sigma2, 10, tolerance) )
      {
      std::cerr.precision( static_cast< int >( itk::Math::abs( std::log10( tolerance ) ) ) );
      std::cerr << "Test FAILED! ";
      std::cerr << "Error in the Sigma values" << std::endl;
      std::cerr << "Expected " << sigma << " but got " << sigma2;
      std::cerr << " along the [" << i << "]-th dimension." << std::endl;
      return EXIT_FAILURE;
      }
    }

  UnsharpMaskImageFilterFilterType::InternalPrecisionType amount = 0.8;
  filter->SetAmount( amount );
  TEST_SET_GET_VALUE( amount, filter->GetAmount() );

  threshold = 0.01;
  filter->SetThreshold( threshold );
  TEST_SET_GET_VALUE( threshold, filter->GetThreshold() );

  bool clamp = itk::NumericTraits<
    UnsharpMaskImageFilterFilterType::OutputPixelType >::IsInteger;
  filter->SetClamp( clamp );
  TEST_SET_GET_VALUE( clamp, filter->GetClamp() );

  if( clamp )
    {
    filter->ClampOn();
    TEST_SET_GET_VALUE( true, filter->GetClamp() );
    }
  else
    {
    filter->ClampOff();
    TEST_SET_GET_VALUE( false, filter->GetClamp() );
    }

  // Execute the filter
  TRY_EXPECT_NO_EXCEPTION( filter->Update() );

  // Get the Smart Pointer to the Filter Output
  // It is important to do it AFTER the filter is Updated
  // Because the object connected to the output may be changed
  // by another during GenerateData() call
  GradientImageType::Pointer outputImage = filter->GetOutput();

  // check that output is correct near the step
  start[0] = 9;
  float mins[4] = { -0.21f, -0.33f, 1.32f, 1.20f };
  float maxs[4] = { -0.20f, -0.32f, 1.33f, 1.21f };
  for( unsigned int i = 0; i < 4; i++ )
    {
    if( outputImage->GetPixel(start) < mins[i]
        || outputImage->GetPixel(start) > maxs[i] )
      {
      std::cerr << "Test FAILED! Unexpected value: ";
      std::cerr << outputImage->GetPixel(start) << std::endl;
      std::cerr << "Expected value between " << mins[i];
      std::cerr << " and " << maxs[i] << std::endl;
      return EXIT_FAILURE;
      }
    ++start[0];
    }

  // All objects should be automatically destroyed at this point
  std::cout << std::endl << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;
}
