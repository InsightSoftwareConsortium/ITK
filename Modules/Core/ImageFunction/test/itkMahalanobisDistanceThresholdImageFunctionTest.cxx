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
#include "itkImageFunction.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

int itkMahalanobisDistanceThresholdImageFunctionTest( int, char* [] )
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

  EXERCISE_BASIC_OBJECT_METHODS( function, MahalanobisDistanceThresholdImageFunction,
    ImageFunction );

  function->SetInputImage( image );

  const double threshold = 5.0;
  function->SetThreshold( threshold );

  FunctionType::CovarianceMatrixType covariance( Dimension, Dimension );
  FunctionType::MeanVectorType mean( Dimension );

  mean[0] = 10.0;
  mean[1] = 20.0;
  mean[2] = 30.0;

  covariance.fill( 0.0 );
  covariance[0][0] = 100.0;
  covariance[1][1] = 200.0;
  covariance[2][2] = 300.0;

  function->SetCovariance( covariance );
  function->SetMean( mean );

  TEST_SET_GET_VALUE( covariance, function->GetCovariance() );
  TEST_SET_GET_VALUE( mean, function->GetMean() );

  ImageType::IndexType index;

  index[0] = 25;
  index[1] = 25;
  index[2] = 25;

  TEST_EXPECT_TRUE( function->EvaluateAtIndex( index ) );

  const double distance = function->EvaluateDistanceAtIndex( index );
  std::cout << "function->EvaluateDistanceAtIndex( index ): " << distance << std::endl;

  const double expectedDistance = 0.244949;
  if( ! itk::Math::FloatAlmostEqual( distance, expectedDistance, 10, 1e-5 ) )
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

  TEST_EXPECT_TRUE( function->Evaluate( point ) );

  const double distance2 = function->EvaluateDistance(point);
  std::cout << "function->EvaluateDistance(point): " << distance2 << std::endl;

  if( ! itk::Math::FloatAlmostEqual( distance2, expectedDistance, 10, 1e-5 ) )
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

  TEST_EXPECT_TRUE( function->EvaluateAtContinuousIndex( cindex ) );

  // Test GetConstReferenceMacro
  const double & getThreshold = function->GetThreshold();
  std::cout << "function->GetThreshold(): " << getThreshold << std::endl;
  if( ! itk::Math::FloatAlmostEqual( threshold, getThreshold, 10, 1e-9 ) )
    {
    std::cerr << "Error: Set/Get Threshold do not match" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;

}
