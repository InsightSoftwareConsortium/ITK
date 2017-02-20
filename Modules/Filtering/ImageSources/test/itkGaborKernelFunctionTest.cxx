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

#include "itkGaborKernelFunction.h"
#include "itkMacro.h"
#include "itkTestingMacros.h"

int itkGaborKernelFunctionTest( int itkNotUsed( argc ), char * itkNotUsed( argv )[] )
{
  typedef itk::GaborKernelFunction< double > KernelFunctionType;
  KernelFunctionType::Pointer gabor = KernelFunctionType::New();

  EXERCISE_BASIC_OBJECT_METHODS( gabor, GaborKernelFunction,
    KernelFunctionBase );

  double sigma = 1.5;
  gabor->SetSigma(sigma);
  TEST_SET_GET_VALUE( sigma, gabor->GetSigma() );

  double frequency = 2.;
  gabor->SetFrequency(frequency);
  TEST_SET_GET_VALUE( frequency, gabor->GetFrequency() );

  double phaseOffset = 0.8;
  gabor->SetPhaseOffset(phaseOffset);
  TEST_SET_GET_VALUE( phaseOffset, gabor->GetPhaseOffset() );

  bool calculateImaginaryPart = true;
  gabor->SetCalculateImaginaryPart( calculateImaginaryPart );
  TEST_SET_GET_VALUE( calculateImaginaryPart, gabor->GetCalculateImaginaryPart() );

  gabor->CalculateImaginaryPartOn();
  TEST_SET_GET_VALUE( true, gabor->GetCalculateImaginaryPart() );

  double tolerance = 1e-12;
  double point = 2.86;
  double expectedValue = -0.13297125073713259;
  double result = gabor->Evaluate( point );
  if( !itk::Math::FloatAlmostEqual( expectedValue, result, 10, tolerance ) )
    {
    std::cout.precision( static_cast< int >( itk::Math::abs( std::log10( tolerance ) ) ) );
    std::cout << "Error in point " << point << ": ";
    std::cout << "Expected: " << expectedValue << ", differs from: "
      << result << std::endl;
    std::cerr << " by more than " << tolerance << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  gabor->CalculateImaginaryPartOff();
  TEST_SET_GET_VALUE( false, gabor->GetCalculateImaginaryPart() );

  expectedValue = 0.093234196962237226;
  result = gabor->Evaluate( point );
  if( !itk::Math::FloatAlmostEqual( expectedValue, result, 10, tolerance ) )
    {
    std::cout.precision( static_cast< int >( itk::Math::abs( std::log10( tolerance ) ) ) );
    std::cout << "Error in point " << point << ": ";
    std::cout << "Expected: " << expectedValue << ", differs from: "
      << result << std::endl;
    std::cerr << " by more than " << tolerance << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
