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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkSimpleFilterWatcher.h"
#include "itkRobustAutomaticThresholdImageFilter.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

int itkRobustAutomaticThresholdImageFilterTest( int argc, char *argv[] )
{

  if( argc != 7 )
    {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << argv[0]
      << " inputImage outputImage pow insideValue outsideValue expectedThreshold"
      << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int Dimension = 2;

  typedef unsigned short                      PixelType;
  typedef itk::Image< PixelType, Dimension >  ImageType;

  typedef float                                   RealPixelType;
  typedef itk::Image< RealPixelType, Dimension >  RealImageType;

  typedef itk::ImageFileReader< ImageType > ReaderType;
  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName( argv[1] );

  typedef itk::GradientMagnitudeRecursiveGaussianImageFilter< ImageType, RealImageType >
    GradientType;
  GradientType::Pointer gradient = GradientType::New();
  gradient->SetInput( reader->GetOutput() );
  gradient->SetSigma( 10 );
  gradient->Update();

  typedef itk::RobustAutomaticThresholdImageFilter< ImageType, RealImageType >
    FilterType;
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter,
    RobustAutomaticThresholdImageFilter, ImageToImageFilter );

  itk::SimpleFilterWatcher watcher( filter, "RobustAutomaticThresholdImageFilter" );


  filter->SetGradientImage( gradient->GetOutput() );

  double pow = atof( argv[3] );
  filter->SetPow( pow );
  TEST_SET_GET_VALUE( pow, filter->GetPow() );

  FilterType::InputPixelType insideValue =
    static_cast< FilterType::InputPixelType >( atof( argv[4] ) );
  filter->SetInsideValue( insideValue );
  TEST_SET_GET_VALUE( insideValue, filter->GetInsideValue() );

  FilterType::OutputPixelType outsideValue =
    static_cast< FilterType::InputPixelType >( atof( argv[5] ) );
  filter->SetOutsideValue( outsideValue );
  TEST_SET_GET_VALUE( outsideValue, filter->GetOutsideValue() );


  filter->SetInput( reader->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( filter->Update() );


  // Regression test
  FilterType::InputPixelType expectedThreshold =
    static_cast< FilterType::InputPixelType >( atof( argv[6] ) );
  FilterType::InputPixelType computedThreshold = filter->GetThreshold();
  if( itk::Math::NotAlmostEquals( expectedThreshold, computedThreshold ) )
    {
    std::cout << "Test failed!" << std::endl;
    std::cout << "Error in GetThreshold()" << std::endl;
    std::cout << "Expected: "
      << static_cast< itk::NumericTraits<
      FilterType::InputPixelType >::PrintType >( expectedThreshold )
      << ", but got: "
      << static_cast< itk::NumericTraits<
      FilterType::InputPixelType >::PrintType >( computedThreshold )
      << std::endl;
    return EXIT_FAILURE;
    }


  typedef itk::ImageFileWriter< ImageType > WriterType;
  WriterType::Pointer writer = WriterType::New();
  writer->SetInput( filter->GetOutput() );
  writer->SetFileName( argv[2] );


  TRY_EXPECT_NO_EXCEPTION( writer->Update() );


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
