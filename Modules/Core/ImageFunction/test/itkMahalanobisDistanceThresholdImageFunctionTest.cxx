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


#include "itkMahalanobisDistanceThresholdImageFunction.h"
#include "itkImage.h"

int itkMahalanobisDistanceThresholdImageFunctionTest(int, char* [] )
{

  const unsigned int                         Dimension = 3;
  typedef unsigned char                      PixelComponentType;
  typedef itk::RGBPixel<PixelComponentType>  PixelType;

  typedef itk::Image< PixelType, Dimension > ImageType;
  typedef itk::MahalanobisDistanceThresholdImageFunction< ImageType >
                                             FunctionType;

  // Create and allocate the image
  ImageType::Pointer      image = ImageType::New();
  ImageType::SizeType     size;
  ImageType::IndexType    start;
  ImageType::RegionType   region;

  size[0] = 50;
  size[1] = 50;
  size[2] = 50;

  start.Fill( 0 );

  region.SetIndex( start );
  region.SetSize( size );

  image->SetRegions( region );
  image->Allocate();

  ImageType::PixelType initialValue;

  initialValue[0] = 11;
  initialValue[1] = 22;
  initialValue[2] = 33;

  image->FillBuffer( initialValue );

  FunctionType::Pointer function = FunctionType::New();

  function->SetInputImage( image );

  const double threshold = 5.0;
  function->SetThreshold( threshold );


  FunctionType::CovarianceMatrixType Covariance( Dimension, Dimension );
  FunctionType::MeanVectorType  Mean( Dimension );

  Mean[0] = 10.0;
  Mean[1] = 20.0;
  Mean[2] = 30.0;

  Covariance.fill( 0.0 );
  Covariance[0][0] = 100.0;
  Covariance[1][1] = 200.0;
  Covariance[2][2] = 300.0;

  function->SetCovariance( Covariance );
  function->SetMean( Mean );

  ImageType::IndexType    index;

  index[0] = 25;
  index[1] = 25;
  index[2] = 25;

  FunctionType::OutputType  belongs;

  belongs = function->EvaluateAtIndex( index );
  std::cout << "function->EvaluateAtIndex( index ): " << belongs << std::endl;
  if( !belongs )
    {
    std::cerr << "Error in EvaluateAtIndex() we were expecting true and got false" << std::endl;
    return EXIT_FAILURE;
    }

  const double distance = function->EvaluateDistanceAtIndex( index );
  std::cout << "function->EvaluateDistanceAtIndex( index ): " << distance << std::endl;

  const double expectedDistance = 0.244949;
  if( std::fabs(distance - expectedDistance) > 1e-5 )
    {
    std::cerr << "Error in distance computation in EvaluateDistanceAtIndex() !!" << std::endl;
    std::cerr << "Expected distance value = " << expectedDistance << std::endl;
    std::cerr << "Distance obtained value = " << distance << std::endl;
    return EXIT_FAILURE;
    }

  // Test Evaluate
  FunctionType::PointType point;
  point[0] = 25;
  point[1] = 25;
  point[2] = 25;
  FunctionType::OutputType belongs2;
  belongs2 = function->Evaluate(point);
  std::cout << "function->Evaluate(point): " << belongs2 << std::endl;

  if( !belongs2 )
    {
    std::cerr << "Error in Evaluate() we were expecting true and got false" << std::endl;
    return EXIT_FAILURE;
    }


  const double distance2 = function->EvaluateDistance(point);
  std::cout << "function->EvaluateDistance(point): " << distance2 << std::endl;

  if( std::fabs(distance2 - expectedDistance) > 1e-5 )
    {
    std::cerr << "Error in distance computation in EvaluateDistance() !!" << std::endl;
    std::cerr << "Expected distance value = " << expectedDistance << std::endl;
    std::cerr << "Distance obtained value = " << distance2 << std::endl;
    return EXIT_FAILURE;
    }


  // Test EvaluateAtContinuousIndex
  FunctionType::ContinuousIndexType cindex;
  cindex[0] = 25;
  cindex[1] = 25;
  cindex[2] = 25;
  FunctionType::OutputType belongs3;
  belongs3 = function->EvaluateAtContinuousIndex(cindex);
  std::cout << "function->EvaluateAtContinuousIndex(cindex): " << belongs3 << std::endl;

  if( !belongs3 )
    {
    std::cerr << "Error in EvaluateAtContinuousIndex() we were expecting true and got false" << std::endl;
    return EXIT_FAILURE;
    }


  // Test GetConstReferenceMacro
  const double & getThreshold = function->GetThreshold();
  std::cout << "function->GetThreshold(): " << getThreshold << std::endl;
  if( std::fabs( threshold - getThreshold ) > 1e-9 )
    {
    std::cerr << "Error: Set/Get Threshold do not match" << std::endl;
    return EXIT_FAILURE;
    }


  // Exercise GetMean() and GetCovariance()
  Mean       = function->GetMean();
  Covariance = function->GetCovariance();


  std::cout << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;

}
