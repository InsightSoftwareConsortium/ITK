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

#include "itkRescaleIntensityImageFilter.h"
#include "itkRandomImageSource.h"
#include "itkTestingMacros.h"
#include "itkUnaryFunctorImageFilter.h"

int itkRescaleIntensityImageFilterTest( int, char* [] )
{
  std::cout << "itkRescaleIntensityImageFilterTest Start" << std::endl;

  // Define the dimension of the images
  const unsigned int ImageDimension = 3;

  // Declare the pixel types of the images
  typedef float                PixelType;

  // Declare the types of the images
  typedef itk::Image< PixelType, ImageDimension > TestInputImage;
  typedef itk::Image< PixelType, ImageDimension > TestOutputImage;

  TestInputImage::RegionType  region;

  TestInputImage::SizeType    size;
  size.Fill(64);

  TestInputImage::IndexType   index;
  index.Fill(0);

  region.SetIndex (index);
  region.SetSize (size);


  typedef itk::RescaleIntensityImageFilter< TestInputImage, TestOutputImage >
    FilterType;

  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, RescaleIntensityImageFilter,
    UnaryFunctorImageFilter );

  // Now generate a real image

  typedef itk::RandomImageSource< TestInputImage > SourceType;
  SourceType::Pointer source = SourceType::New();

  TestInputImage::SizeValueType randomSize[3] = {17, 8, 20};

  // Set up source
  source->SetSize( randomSize );
  double minValue = -128.0;
  double maxValue = 127.0;

  source->SetMin( static_cast< TestInputImage::PixelType >( minValue ) );
  source->SetMax( static_cast< TestInputImage::PixelType >( maxValue ) );

  filter->SetFunctor( filter->GetFunctor() );
  filter->SetInput( source->GetOutput() );

  const double desiredMinimum = -1.0;
  const double desiredMaximum =  1.0;

  filter->SetOutputMinimum( desiredMinimum );
  TEST_SET_GET_VALUE( desiredMinimum, filter->GetOutputMinimum() );

  filter->SetOutputMaximum( desiredMaximum );
  TEST_SET_GET_VALUE( desiredMaximum, filter->GetOutputMaximum() );

  TRY_EXPECT_NO_EXCEPTION( filter->UpdateLargestPossibleRegion() );

  typedef itk::MinimumMaximumImageCalculator< TestOutputImage > CalculatorType;
  CalculatorType::Pointer calculator = CalculatorType::New();

  calculator->SetImage( filter->GetOutput() );

  calculator->Compute();

  const double tolerance = 1e-7;

  const double obtainedMinimum = calculator->GetMinimum();
  const double obtainedMaximum = calculator->GetMaximum();

  if( !itk::Math::FloatAlmostEqual( obtainedMinimum, desiredMinimum, 10, tolerance ) )
    {
    std::cerr << "Error in minimum" << std::endl;
    std::cerr << "Expected minimum = " << desiredMinimum  << std::endl;
    std::cerr << "Obtained minimum = " << obtainedMinimum << std::endl;
    return EXIT_FAILURE;
    }

  if( !itk::Math::FloatAlmostEqual( obtainedMaximum, desiredMaximum, 10, tolerance ) )
    {
    std::cerr << "Error in minimum" << std::endl;
    std::cerr << "Expected minimum = " << desiredMaximum  << std::endl;
    std::cerr << "Obtained minimum = " << obtainedMaximum << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;
}
