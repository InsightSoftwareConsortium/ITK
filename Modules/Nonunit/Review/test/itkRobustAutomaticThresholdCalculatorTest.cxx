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

#include "itkRobustAutomaticThresholdCalculator.h"
#include "itkGradientMagnitudeRecursiveGaussianImageFilter.h"
#include "itkMath.h"
#include "itkTestingMacros.h"


int itkRobustAutomaticThresholdCalculatorTest( int argc, char *argv[] )
{
  if( argc != 4 )
    {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << argv[0] << " inputImage pow expectedOutput" << std::endl;
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

  typedef itk::RobustAutomaticThresholdCalculator< ImageType, RealImageType >
    CalculatorType;
  CalculatorType::Pointer calculator = CalculatorType::New();

  EXERCISE_BASIC_OBJECT_METHODS( calculator,
    RobustAutomaticThresholdCalculator, Object );


  calculator->SetGradient( gradient->GetOutput() );

  double pow = atof( argv[2] );
  calculator->SetPow( pow );
  TEST_SET_GET_VALUE( pow, calculator->GetPow() );


  // Test input or gradient unset exceptions
  TRY_EXPECT_EXCEPTION( calculator->Compute() );

  TRY_EXPECT_EXCEPTION( calculator->GetOutput() );


  calculator->SetInput( reader->GetOutput() );

  TRY_EXPECT_NO_EXCEPTION( calculator->Compute() );


  // Regression test
  CalculatorType::InputPixelType expectedOutput =
    static_cast< CalculatorType::InputPixelType >( atof( argv[3] ) );
  CalculatorType::InputPixelType computedOutput = calculator->GetOutput();
  if( itk::Math::NotAlmostEquals( expectedOutput, computedOutput ) )
    {
    std::cout << "Test failed!" << std::endl;
    std::cout << "Error in GetOutput()" << std::endl;
    std::cout << "Expected: "
      << static_cast< itk::NumericTraits<
      CalculatorType::InputPixelType >::PrintType >( expectedOutput )
      << ", but got: "
      << static_cast< itk::NumericTraits<
      CalculatorType::InputPixelType >::PrintType >( computedOutput )
      << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
