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


#include "itkCovarianceImageFunction.h"
#include "itkImageFunction.h"
#include "itkTestingMacros.h"

int itkCovarianceImageFunctionTest( int, char* [] )
{

  const unsigned int Dimension = 3;
  typedef unsigned char PixelComponentType;
  const unsigned int VectorDimension = 4;

  typedef itk::FixedArray< PixelComponentType, VectorDimension > PixelType;
  typedef itk::Image< PixelType, Dimension >                     ImageType;
  typedef itk::CovarianceImageFunction< ImageType >              FunctionType;

  // Create and allocate the image
  ImageType::Pointer      image = ImageType::New();
  ImageType::SizeType     size;
  ImageType::IndexType    start;
  ImageType::RegionType   region;

  size[0] = 20;
  size[1] = 20;
  size[2] = 20;

  ImageType::IndexValueType imageValue = 0;
  start.Fill( imageValue );

  region.SetIndex( start );
  region.SetSize( size );

  image->SetRegions( region );
  image->Allocate();

  ImageType::PixelType initialValue;

  initialValue[0] = 11;
  initialValue[1] = 13;
  initialValue[2] = 17;
  initialValue[3] = 19;

  image->FillBuffer( initialValue );

  FunctionType::Pointer function = FunctionType::New();
  EXERCISE_BASIC_OBJECT_METHODS( function, CovarianceImageFunction,
    ImageFunction );

  function->SetInputImage( image );

  unsigned int neighborhoodRadius = 5;
  function->SetNeighborhoodRadius( neighborhoodRadius );
  TEST_SET_GET_VALUE( neighborhoodRadius, function->GetNeighborhoodRadius() );

  ImageType::IndexType index;

  index[0] = 10;
  index[1] = 10;
  index[2] = 10;

  FunctionType::OutputType covariance;

  covariance = function->EvaluateAtIndex( index );
  std::cout << "function->EvaluateAtIndex( index ): " << covariance << std::endl;

  // Test Evaluate
  FunctionType::PointType point;
  point[0] = 25;
  point[1] = 25;
  point[2] = 25;
  FunctionType::OutputType covariance2;
  covariance2 = function->Evaluate(point);
  std::cout << "function->Evaluate(point): " << covariance2 << std::endl;

  // Test EvaluateAtContinuousIndex
  FunctionType::ContinuousIndexType cindex;
  cindex[0] = 25;
  cindex[1] = 25;
  cindex[2] = 25;
  FunctionType::OutputType covariance3;
  covariance3 = function->EvaluateAtContinuousIndex(cindex);
  std::cout << "function->EvaluateAtContinuousIndex(cindex): " << covariance3 << std::endl;


  // Since the input image is constant, the covariance should be equal to
  // the initial value
  for( unsigned int ix = 0; ix < VectorDimension; ++ix )
    {
    for( unsigned int iy = 0; iy < VectorDimension; ++iy )
      {
      // Covariance must be zero in this image with constant values
      if( ! itk::Math::FloatAlmostEqual( itk::Math::abs( covariance[ix][iy] ),
        static_cast< FunctionType::OutputType::element_type >( imageValue ), 10, 10e-7 ) )
        {
        std::cerr << "Error in covariance computation" << std::endl;
        return EXIT_FAILURE;
        }
      }
    }

  std::cout << "Test PASSED ! " << std::endl;
  return EXIT_SUCCESS;
}
