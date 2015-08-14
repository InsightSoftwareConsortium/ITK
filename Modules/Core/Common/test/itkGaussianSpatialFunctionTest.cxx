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

int itkGaussianSpatialFunctionTest(int, char* [] )
{
  // Change this parameter (and the positions, below) to work in higher or lower dimensions
  const unsigned int Dimension = 3;

  //---------Create and initialize a spatial function-----------

  typedef itk::GaussianSpatialFunction<double,Dimension> FunctionType;

  typedef FunctionType::ArrayType ArrayType;
  typedef FunctionType::InputType InputType;

  // Create and initialize a new sphere function

  FunctionType::Pointer spatialFunction = FunctionType::New();

  ArrayType mean;
  mean[0]=13;
  mean[1]=17;
  mean[2]=19;
  spatialFunction->SetMean( mean );

  // Test the Get macros as well
  ArrayType mean1 = spatialFunction->GetMean();
  if( mean1 != mean )
    {
    return EXIT_FAILURE;
    }
  // FIXME : verify the return values...

  ArrayType sigma;
  sigma[0]=5;
  sigma[1]=7;
  sigma[2]=9;
  spatialFunction->SetSigma( sigma );

  // Test the Get macros as well
  ArrayType sigma1 = spatialFunction->GetSigma();
  if( sigma1 != sigma )
    {
    return EXIT_FAILURE;
    }
  // FIXME : verify the return values...

  double scale1 = spatialFunction->GetScale();
  if( std::fabs( scale1 - 1.0 ) > vnl_math::eps )
    {
    std::cerr << "Error in initial scale value" << std::endl;
    return EXIT_FAILURE;
    }


  bool normalized1 = spatialFunction->GetNormalized();
  if( normalized1 )
    {
    std::cerr << "Error in initial value of normalized" << std::endl;
    return EXIT_FAILURE;
    }


  double scale2 = 19.0;
  spatialFunction->SetScale( scale2 );
  if( itk::Math::NotExactlyEquals(spatialFunction->GetScale(), scale2) )
    {
    std::cerr << "Error in Set/GetScale()" << std::endl;
    return EXIT_FAILURE;
    }

  spatialFunction->SetScale( 1.0 );
  spatialFunction->SetNormalized( true );


  std::cout << "Gaussian spatial function created\n";

  //----------------Test evaluation of funtion------------------

  // We're going to evaluate it at the center of the Gaussian (10,10,10)
  InputType point;
  point[0] = mean[0];
  point[1] = mean[1];
  point[2] = mean[2];

  std::cout << spatialFunction->GetNameOfClass() << std::endl;
  spatialFunction->Print( std::cout );

  double computedValueAtMean = spatialFunction->Evaluate( point );
  std::cout << "Gaussian function value is " << computedValueAtMean << std::endl;

  const double oneDimensionalFactor = std::sqrt( 2.0 * vnl_math::pi );
  const double factor = oneDimensionalFactor * oneDimensionalFactor * oneDimensionalFactor;
  double expectedValueAtMean = 1.0 / ( sigma[0]*sigma[1]*sigma[2] * factor );

  std::cout << "expectedValueAtMean = " << expectedValueAtMean << std::endl;
  std::cout << "computed value      = " << computedValueAtMean << std::endl;

  if( std::fabs( computedValueAtMean - expectedValueAtMean ) > vnl_math::eps )
    {
    std::cerr << "Error in computation of value at mean" << std::endl;
    return EXIT_FAILURE;
    }


  return EXIT_SUCCESS;
}
