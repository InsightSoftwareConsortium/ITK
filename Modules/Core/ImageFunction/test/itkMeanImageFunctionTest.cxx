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


#include "itkMeanImageFunction.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

int itkMeanImageFunctionTest( int, char* [] )
{
  int testStatus = EXIT_SUCCESS;

  const unsigned int Dimension = 3;
  typedef unsigned char   PixelType;

  typedef itk::Image< PixelType, Dimension >  ImageType;
  typedef itk::MeanImageFunction< ImageType > FunctionType;

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

  ImageType::PixelType initialValue = 27;

  image->FillBuffer( initialValue );

  FunctionType::Pointer function = FunctionType::New();

  EXERCISE_BASIC_OBJECT_METHODS( function, MeanImageFunction, ImageFunction );

  function->SetInputImage( image );

  unsigned int neighborhoodRadius = 5;
  function->SetNeighborhoodRadius( neighborhoodRadius );
  TEST_SET_GET_VALUE( neighborhoodRadius, function->GetNeighborhoodRadius() );

  ImageType::IndexType index;

  index[0] = 25;
  index[1] = 25;
  index[2] = 25;

  FunctionType::RealType mean;
  mean = function->EvaluateAtIndex( index );

  double epsilon = 1e-7;
  if( !itk::Math::FloatAlmostEqual( static_cast< FunctionType::RealType >(initialValue), mean, 10, epsilon ) )
    {
    std::cout.precision( unsigned( itk::Math::abs( std::log10( epsilon ) ) ) );
    std::cout << "Mean value (" << mean << ") does not equal initialValue ("
              << initialValue << ")" << std::endl;
    testStatus = EXIT_FAILURE;
    }

  // Test Evaluate
  FunctionType::PointType point;
  point[0] = 25;
  point[1] = 25;
  point[2] = 25;
  FunctionType::RealType mean2;
  mean2 = function->Evaluate(point);

  if( !itk::Math::FloatAlmostEqual( static_cast< FunctionType::RealType >(initialValue), mean2, 10, epsilon ) )
    {
    std::cout.precision( unsigned( itk::Math::abs( std::log10( epsilon ) ) ) );
    std::cout << "Mean value (" << mean2 << ") does not equal initialValue ("
              << initialValue << ")" << std::endl;
    testStatus = EXIT_FAILURE;
    }

  // Test EvaluateAtContinuousIndex
  FunctionType::ContinuousIndexType cindex;
  cindex[0] = 25;
  cindex[1] = 25;
  cindex[2] = 25;
  FunctionType::RealType mean3;
  mean3 = function->EvaluateAtContinuousIndex(cindex);

  if( !itk::Math::FloatAlmostEqual( static_cast< FunctionType::RealType >(initialValue), mean3, 10, epsilon ) )
    {
    std::cout.precision( unsigned( itk::Math::abs( std::log10( epsilon ) ) ) );
    std::cout << "Mean value (" << mean3 << ") does not equal initialValue ("
              << initialValue << ")" << std::endl;
    testStatus = EXIT_FAILURE;
    }

  return testStatus;
}
