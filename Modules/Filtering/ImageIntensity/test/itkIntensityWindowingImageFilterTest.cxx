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

#include "itkIntensityWindowingImageFilter.h"
#include "itkMath.h"
#include "itkNumericTraits.h"
#include "itkRandomImageSource.h"
#include "itkTestingMacros.h"

int itkIntensityWindowingImageFilterTest( int, char* [] )
{
  const unsigned int Dimension = 3;
  typedef float PixelType;

  typedef itk::Image< PixelType, Dimension > TestInputImage;
  typedef itk::Image< PixelType, Dimension > TestOutputImage;

  TestInputImage::RegionType region;

  TestInputImage::SizeType   size;
  size.Fill(64);

  TestInputImage::IndexType  index;
  index.Fill(0);

  region.SetIndex (index);
  region.SetSize (size);


  typedef itk::IntensityWindowingImageFilter< TestInputImage, TestOutputImage > FilterType;
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, IntensityWindowingImageFilter,
    UnaryFunctorImageFilter );

  // Generate a real image
  typedef itk::RandomImageSource< TestInputImage > SourceType;
  SourceType::Pointer source = SourceType::New();
  TestInputImage::SizeValueType randomSize[3] = {17, 8, 20};

  // Set up source
  source->SetSize( randomSize );

  double minValue = -128.0;
  double maxValue =  127.0;

  source->SetMin( static_cast< TestInputImage::PixelType >( minValue ) );
  source->SetMax( static_cast< TestInputImage::PixelType >( maxValue ) );

  filter->SetInput( source->GetOutput() );

  const double desiredMinimum = -1.0;
  const double desiredMaximum =  1.0;

  const float  windowMinimum = -50.0f;
  const float  windowMaximum =  50.0f;

  filter->SetOutputMinimum( desiredMinimum );
  TEST_SET_GET_VALUE( desiredMinimum, filter->GetOutputMinimum() );

  filter->SetOutputMaximum( desiredMaximum );
  TEST_SET_GET_VALUE( desiredMaximum, filter->GetOutputMaximum() );

  filter->SetWindowMinimum( windowMinimum );
  TEST_SET_GET_VALUE( windowMinimum, filter->GetWindowMinimum() );

  filter->SetWindowMaximum( windowMaximum );
  TEST_SET_GET_VALUE( windowMaximum, filter->GetWindowMaximum() );

  std::cout << "Window minimum:maximum = "
    << windowMinimum << ":" << windowMaximum
    << ", equivalent window:level = "
    << static_cast< itk::NumericTraits<
    FilterType::InputPixelType >::PrintType >( filter->GetWindow() )
    << ":"
    << static_cast< itk::NumericTraits<
    FilterType::InputPixelType >::PrintType >( filter->GetLevel() )
    << std::endl;

  std::cout << "Gray level linear transformation scale = "
    << static_cast< itk::NumericTraits<
    FilterType::RealType >::PrintType >( filter->GetScale() )
    << ", shift = "
    << static_cast< itk::NumericTraits<
    FilterType::RealType >::PrintType >( filter->GetShift() )
    << std::endl;

  TRY_EXPECT_NO_EXCEPTION( filter->UpdateLargestPossibleRegion() );

  TRY_EXPECT_NO_EXCEPTION( filter->SetFunctor( filter->GetFunctor() ) );

  typedef itk::MinimumMaximumImageCalculator< TestOutputImage > CalculatorType;
  CalculatorType::Pointer calculator = CalculatorType::New();

  calculator->SetImage( filter->GetOutput() );

  calculator->Compute();

  const double tolerance = 1e-7;

  const double obtainedMinimum = calculator->GetMinimum();
  const double obtainedMaximum = calculator->GetMaximum();

  if( !itk::Math::FloatAlmostEqual( obtainedMinimum, desiredMinimum, 10, tolerance ) )
    {
    std::cerr.precision( static_cast< int >( itk::Math::abs( std::log10( tolerance ) ) ) );
    std::cerr << "Error in minimum" << std::endl;
    std::cerr << "Expected minimum = " << desiredMinimum  << std::endl;
    std::cerr << "Obtained minimum = " << obtainedMinimum << std::endl;
    return EXIT_FAILURE;
    }

  if( !itk::Math::FloatAlmostEqual( obtainedMaximum, desiredMaximum, 10, tolerance ) )
    {
    std::cerr.precision( static_cast< int >( itk::Math::abs( std::log10( tolerance ) ) ) );
    std::cerr << "Error in maximum" << std::endl;
    std::cerr << "Expected maximum = " << desiredMaximum  << std::endl;
    std::cerr << "Obtained maximum = " << obtainedMaximum << std::endl;
    return EXIT_FAILURE;
    }

  const float window = 50.0f;
  const float level  = 50.0f;

  filter->SetWindowLevel( window, level );

  std::cout << "Window:level = "
            << static_cast< itk::NumericTraits<
            FilterType::InputPixelType >::PrintType >( filter->GetWindow() )
            << ":"
            << static_cast< itk::NumericTraits<
            FilterType::InputPixelType >::PrintType >( filter->GetLevel() )
            << ", equivalent window minimum:maximum = "
            << static_cast< itk::NumericTraits<
            FilterType::InputPixelType >::PrintType >( filter->GetWindowMinimum() )
            << ":"
            << static_cast< itk::NumericTraits<
            FilterType::InputPixelType >::PrintType >( filter->GetWindowMaximum() )
            << std::endl;

  TRY_EXPECT_NO_EXCEPTION( filter->UpdateLargestPossibleRegion() );

  calculator->Compute();

  const double obtainedMinimum2 = calculator->GetMinimum();
  const double obtainedMaximum2 = calculator->GetMaximum();

  if( !itk::Math::FloatAlmostEqual( obtainedMinimum2, desiredMinimum, 10, tolerance ) )
    {
    std::cerr.precision( static_cast< int >( itk::Math::abs( std::log10( tolerance ) ) ) );
    std::cerr << "Error in minimum" << std::endl;
    std::cerr << "Expected minimum = " << desiredMinimum   << std::endl;
    std::cerr << "Obtained minimum = " << obtainedMinimum2 << std::endl;
    return EXIT_FAILURE;
    }

  if( !itk::Math::FloatAlmostEqual( obtainedMaximum2, desiredMaximum, 10, tolerance ) )
    {
    std::cerr.precision( static_cast< int >( itk::Math::abs( std::log10( tolerance ) ) ) );
    std::cerr << "Error in maximum" << std::endl;
    std::cerr << "Expected maximum = " << desiredMaximum   << std::endl;
    std::cerr << "Obtained maximum = " << obtainedMaximum2 << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;
}
