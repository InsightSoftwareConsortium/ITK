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

#include "itkGaussianSpatialFunction.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

int itkGaussianSpatialFunctionTest( int argc, char* argv[] )
{
  if ( argc < 3 )
    {
    std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv)
      << " scale normalized" << std::endl;
    return EXIT_FAILURE;
    }

  constexpr unsigned int Dimension = 3;
  using PixeltType = double;

  using GaussianSpatialFunctionType = itk::GaussianSpatialFunction< PixeltType, Dimension >;

  using ArrayType = GaussianSpatialFunctionType::ArrayType;
  using InputType = GaussianSpatialFunctionType::InputType;

  // Create and initialize the Spatial function

  GaussianSpatialFunctionType::Pointer gaussianSpatialFunction =
    GaussianSpatialFunctionType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS( gaussianSpatialFunction, GaussianSpatialFunction,
    SpatialFunction );

  ArrayType mean;
  mean[0] = 13;
  mean[1] = 17;
  mean[2] = 19;
  gaussianSpatialFunction->SetMean( mean );
  ITK_TEST_SET_GET_VALUE( mean, gaussianSpatialFunction->GetMean() );

  ArrayType sigma;
  sigma[0] = 5;
  sigma[1] = 7;
  sigma[2] = 9;
  gaussianSpatialFunction->SetSigma( sigma );
  ITK_TEST_SET_GET_VALUE( sigma, gaussianSpatialFunction->GetSigma() );

  double scale = std::stod( argv[1] );
  gaussianSpatialFunction->SetScale( scale );
  ITK_TEST_SET_GET_VALUE( scale, gaussianSpatialFunction->GetScale() );

  auto normalized = static_cast< bool >( std::stoi( argv[2] ) );
  gaussianSpatialFunction->SetNormalized( normalized );
  ITK_TEST_SET_GET_VALUE( normalized, gaussianSpatialFunction->GetNormalized() );

  if( normalized )
    {
    gaussianSpatialFunction->NormalizedOn();
    ITK_TEST_SET_GET_VALUE( true, gaussianSpatialFunction->GetNormalized() );
    }
  else
    {
    gaussianSpatialFunction->NormalizedOff();
    ITK_TEST_SET_GET_VALUE( false, gaussianSpatialFunction->GetNormalized() );
    }


  // Test the evaluation of the Gaussian spatial funtion
  //

  // Evaluate it at the center of the Gaussian
  InputType point;
  point[0] = mean[0];
  point[1] = mean[1];
  point[2] = mean[2];

  double computedValueAtMean = gaussianSpatialFunction->Evaluate( point );

  double expectedValueAtMean = 1.0;
  if( gaussianSpatialFunction->GetNormalized() )
    {
    const double oneDimensionalFactor = std::sqrt( 2.0 * itk::Math::pi );
    const double factor = oneDimensionalFactor * oneDimensionalFactor * oneDimensionalFactor;
    expectedValueAtMean = scale / ( sigma[0]*sigma[1]*sigma[2] * factor );
    }
  else
    {
    constexpr double oneDimensionalFactor = 1.0;
    const double factor = oneDimensionalFactor * oneDimensionalFactor * oneDimensionalFactor;
    expectedValueAtMean = scale / factor;
    }

  if( itk::Math::NotAlmostEquals( expectedValueAtMean, computedValueAtMean ) )
    {
    std::cout << "Error in point " << point << ": ";
    std::cout << "expected: " << expectedValueAtMean << ", but got "
      << computedValueAtMean << std::endl;
    std::cout << "Test failed" << std::endl;
    return EXIT_FAILURE;
    }

  return EXIT_SUCCESS;
}
