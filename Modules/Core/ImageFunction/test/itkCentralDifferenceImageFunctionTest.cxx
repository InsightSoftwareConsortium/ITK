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

#include "itkCentralDifferenceImageFunction.h"
#include "itkImageRegionIterator.h"
#include "itkMath.h"

int itkCentralDifferenceImageFunctionTest(int, char* [] )
{
  int result = EXIT_SUCCESS;

  const unsigned int                            ImageDimension = 2;
  typedef unsigned int                          PixelType;
  typedef itk::Image<PixelType,ImageDimension>  ImageType;

  ImageType::Pointer image = ImageType::New();
  ImageType::SizeType size;
  size.Fill( 16 );
  ImageType::RegionType region( size );

  image->SetRegions( region );
  image->Allocate();

  // make a test image
  typedef itk::ImageRegionIterator<ImageType> Iterator;
  Iterator iter( image, region );
  iter.GoToBegin();
  unsigned int counter = 0;

  while ( !iter.IsAtEnd() )
    {
    iter.Set( counter * counter );
    ++counter;
    ++iter;
    }

  // set up central difference calculator
  typedef float CoordRepType;
  typedef itk::CentralDifferenceImageFunction<ImageType,CoordRepType> FunctionType;
  typedef FunctionType::OutputType                                    OutputType;
  typedef FunctionType::OutputValueType                               OutputValueType;

  FunctionType::Pointer function = FunctionType::New();

  function->SetInputImage( image );

  ImageType::IndexType index;

  // pick an index inside the image
  index.Fill( 8 );
  OutputType indexOutput = function->EvaluateAtIndex( index );
  std::cout << "Index: " << index << " Derivative: ";
  std::cout << indexOutput << std::endl;

  if ( function->IsInsideBuffer( index ) )
    {
    std::cout << "Index: " << index << " is inside the BufferedRegion." << std::endl;
    }

  FunctionType::ContinuousIndexType cindex;
  cindex.Fill( 8.0 );
  OutputType continuousIndexOutput = function->EvaluateAtContinuousIndex( cindex );
  std::cout << "ContinuousIndex: " << cindex << " Derivative: ";
  std::cout << continuousIndexOutput << std::endl;

  if( indexOutput != continuousIndexOutput )
    {
    std::cerr << "ERROR: Output of EvaluateAtIndex and EvaluateAtContinuousIndex do not match." << std::endl;
    result = EXIT_FAILURE;
    }

  FunctionType::PointType point;
  point.Fill( 8.0 );
  OutputType pointOutput = function->Evaluate( point );
  std::cout << "Point: " << point << " Derivative: ";
  std::cout << pointOutput << std::endl;

  // this should be the same as output from EvaluateAtIndex as long as
  // image is setup with default spatial information.
  if( indexOutput != pointOutput )
    {
    std::cerr << "ERROR: Output of EvaluateAtIndex and Evaluate do not match." << std::endl;
    std::cerr << "indexOutput: " << indexOutput << " pointOutput: " << pointOutput << std::endl;
    result = EXIT_FAILURE;
    }

  // pick an index on the image edge
  index.Fill( 8 );
  index[0] = 15;
  indexOutput = function->EvaluateAtIndex( index );
  std::cout << "Index: " << index << " Derivative: ";
  std::cout << indexOutput << std::endl;
  if ( function->IsInsideBuffer( index ) )
    {
    std::cout << "Index: " << index << " is inside the BufferedRegion." << std::endl;
    }
  if( itk::Math::NotExactlyEquals(indexOutput[0], itk::NumericTraits<OutputValueType>::ZeroValue()) )
    {
    std::cout << "ERROR: Index: " << index << " - expected result dim 0 to have value 0. " << std::endl;
    result = EXIT_FAILURE;
    }

  cindex.Fill( 8.0 );
  cindex[0] = 15.0;
  continuousIndexOutput = function->EvaluateAtContinuousIndex( cindex );
  std::cout << "ContinuousIndex: " << cindex << " Derivative: ";
  std::cout << continuousIndexOutput << std::endl;

  if( indexOutput != continuousIndexOutput )
    {
    std::cerr << "ERROR: Output of EvaluateAtIndex and EvaluateAtContinuousIndex do not match at boundary." << std::endl;
    std::cerr << "indexOutput: " << indexOutput << " continuousIndexOutput: " << continuousIndexOutput << std::endl;
    result = EXIT_FAILURE;
    }

  point.Fill( 8.0 );
  point[0] = 15.0;
  pointOutput = function->Evaluate( point );
  std::cout << "Point: " << point << " Derivative: ";
  std::cout << pointOutput << std::endl;

  if( indexOutput != pointOutput )
    {
    std::cerr << "ERROR: Output of EvaluateAtIndex and Evaluate do not match at boundary." << std::endl;
    std::cerr << "indexOutput: " << indexOutput << " pointOutput: " << pointOutput << std::endl;
    result = EXIT_FAILURE;
    }

  // test other edge
  index.Fill( 8 );
  index[1] = 0;
  indexOutput = function->EvaluateAtIndex( index );
  std::cout << "Index: " << index << " Derivative: ";
  std::cout << indexOutput << std::endl;
  if ( function->IsInsideBuffer( index ) )
    {
    std::cout << "Index: " << index << " is inside the BufferedRegion." << std::endl;
    }
  if( itk::Math::NotExactlyEquals(indexOutput[1], itk::NumericTraits<OutputValueType>::ZeroValue()) )
    {
    std::cout << "ERROR: Index: " << index << " - expected result dim 1 to have value 0. " << std::endl;
    result = EXIT_FAILURE;
    }

  cindex.Fill( 8.0 );
  cindex[1] = 0;
  continuousIndexOutput = function->EvaluateAtContinuousIndex( cindex );
  std::cout << "ContinuousIndex: " << cindex << " Derivative: ";
  std::cout << continuousIndexOutput << std::endl;

  if( indexOutput != continuousIndexOutput )
    {
    std::cerr << "ERROR: Output of EvaluateAtIndex and EvaluateAtContinuousIndex do not match at boundary." << std::endl;
    std::cerr << "indexOutput: " << indexOutput << " continuousIndexOutput: " << continuousIndexOutput << std::endl;
    result = EXIT_FAILURE;
    }

  point.Fill( 8.0 );
  // The point has to be just off of 0 because of the fact that points span +/- 0.5 in space.
  // If just use 0.0, then the test for being on a boundary will fail because one of the
  // neighboring points will be considered to be the same as point.
  point[1] = -0.000001;
  pointOutput = function->Evaluate( point );
  std::cout << "Point: " << point << " Derivative: ";
  std::cout << pointOutput << std::endl;

  if( indexOutput != pointOutput )
    {
    std::cerr << "ERROR: Output of EvaluateAtIndex and Evaluate do not match at boundary." << std::endl;
    std::cerr << "indexOutput: " << indexOutput << " pointOutput: " << pointOutput << std::endl;
    result = EXIT_FAILURE;
    }

  // DO NOT test out-of-bounds index.
  // Method documentation states that index/point is assumed
  // to be in bounds.

  // test results at non-interger positions
  std::cout << "Test non-integer position for EvaluateAtContinuousIndex. "
            << std::endl;
  cindex.Fill( 8.0 );
  cindex[0] = 8.0;
  OutputType center = function->EvaluateAtContinuousIndex( cindex );
  std::cout << "ContinuousIndex: " << cindex << " Derivative: ";
  std::cout << center << std::endl;
  cindex[0] = 7.5;
  OutputType left = function->EvaluateAtContinuousIndex( cindex );
  std::cout << "ContinuousIndex: " << cindex << " Derivative: ";
  std::cout << left << std::endl;
  cindex[0] = 8.5;
  OutputType right = function->EvaluateAtContinuousIndex( cindex );
  std::cout << "ContinuousIndex: " << cindex << " Derivative: ";
  std::cout << right << std::endl;

  if( center == left || center == right )
    {
    std::cerr << "ERROR: Failed for EvaluateAtContinuousIndex at non-interger indecies. "
              << "Results are unexpectedly identical." << std::endl;
    result = EXIT_FAILURE;
    }

  if( fabs( ( right[0] + left[0] ) / 2.0 - center[0] ) > 1e-06 )
    {
    std::cerr << "ERROR: Failed for EvaluateAtContinuousIndex at non-integer incecies. "
              << "Center index result is not average of left and right."
              << std::endl;
    result = EXIT_FAILURE;
    }

  std::cout << "Test non-integer position for Evaluate. "
            << std::endl;
  point.Fill( 8.0 );
  point[0] = 8.0;
  center = function->Evaluate( point );
  std::cout << "Point: " << point << " Derivative: " << center << std::endl;
  point[0] = 7.5;
  left = function->Evaluate( point );
  std::cout << "Point: " << point << " Derivative: " << left << std::endl;
  point[0] = 8.5;
  right = function->Evaluate( point );
  std::cout << "Point: " << point << " Derivative: " << right << std::endl;

  if( center == left || center == right )
    {
    std::cerr << "ERROR: Failed for Evaluate at non-interger indecies. "
              << "Results are unexpectedly identical." << std::endl;
    result = EXIT_FAILURE;
    }

  if( fabs( ( right[0] + left[0] ) / 2.0 - center[0] ) > 1e-06 )
    {
    std::cerr << "ERROR: Failed for Evaluate at non-integer incecies. "
              << "Center index result is not average of left and right."
              << std::endl;
    result = EXIT_FAILURE;
    }

  // test that a point just at a boundary will yield 0 derivative for
  // the dimension at a boundary
  point.Fill( 8.0 );
  // points are evaluated at the center of the voxel, so [15.1,8] is a valid
  // point, but lies on the boundary because adding 0.5 to it put it > 15.5
  point[0] = 15.1;
  pointOutput = function->Evaluate( point );
  std::cout << "Point: " << point << " Derivative: " << pointOutput << std::endl;

  if( itk::Math::NotExactlyEquals(pointOutput[0], 0) || itk::Math::ExactlyEquals(pointOutput[1], 0) )
    {
    std::cerr << "ERROR: Output of Evaluate just on boundary is not zero." << std::endl;
    std::cerr << " pointOutput: " << pointOutput << std::endl;
    result = EXIT_FAILURE;
    }

  point.Fill( 8.0 );
  OutputType origDerivative = function->Evaluate( point );
  std::cout << "Point: " << point << " origDerivative: " << origDerivative << std::endl;

  // test image direction and Evaluate
  ImageType::DirectionType direction;
  direction[0][0] = -1.0;
  direction[0][1] = 0.0;
  direction[1][0] = 0.0;
  direction[1][1] = -1.0;
  point.Fill( -8.0 );
  image->SetDirection( direction );
  function->SetUseImageDirection( true );
  // first set to null and then reset image so that cached
  // info is recalculated
  function->SetInputImage( ITK_NULLPTR );
  function->SetInputImage( image );
  OutputType directionOnDerivative = function->Evaluate( point );
  std::cout << "Point: " << point << " directionOnDerivative: "
            << directionOnDerivative << std::endl;

  if( itk::Math::NotExactlyEquals(directionOnDerivative[0], -origDerivative[0]) || itk::Math::NotExactlyEquals(directionOnDerivative[1], -origDerivative[1]) )
    {
    std::cerr << "ERROR: Expected origDerivative and directionOnDerivative to be opposite." << std::endl;
    result = EXIT_FAILURE;
    }

  // with image direction disabled, result should be same as with
  // identity direction
  function->SetUseImageDirection( false );
  OutputType directionOffDerivative = function->Evaluate( point );
  std::cout << "Point: " << point << " directionOffDerivative: " << directionOffDerivative << std::endl;

  if( directionOffDerivative != origDerivative )
    {
    std::cerr << "Expected origDerivative == directionOffDerivative."
              << std::endl;
    result = EXIT_FAILURE;
    }

  // test with one negative dimension
  direction[0][0] = 1.0;
  direction[0][1] = 0.0;
  direction[1][0] = 0.0;
  direction[1][1] = -1.0;
  point[0] = 8.0;
  point[1] = -8.0;
  image->SetDirection( direction );
  function->SetUseImageDirection( true );
  // first set to null and then reset image so that cached
  // info is recalculated
  function->SetInputImage( ITK_NULLPTR );
  function->SetInputImage( image );
  OutputType directionOneNegDerivative = function->Evaluate( point );
  std::cout << "Point: " << point << " directionOneNegDerivative: " << directionOneNegDerivative << std::endl;

  if( itk::Math::NotExactlyEquals(directionOneNegDerivative[0], origDerivative[0]) || itk::Math::NotExactlyEquals(directionOneNegDerivative[1], -origDerivative[1]) )
    {
    std::cerr << "ERROR: Expected origDerivative and directionOneNegDerivative to be opposite." << std::endl;
    result = EXIT_FAILURE;
    }

  // test with image direction that swaps dimensions
  direction[0][0] = 0.0;
  direction[0][1] = -1.0;
  direction[1][0] = 1.0;
  direction[1][1] = 0.0;
  point[0] = -8.0;
  point[1] = 8.0;
  image->SetDirection( direction );
  function->SetUseImageDirection( true );
  // first set to null and then reset image so that cached
  // info is recalculated
  function->SetInputImage( ITK_NULLPTR );
  function->SetInputImage( image );
  OutputType directionSwapDerivative = function->Evaluate( point );
  std::cout << "Point: " << point << " directionSwapDerivative: " << directionSwapDerivative << std::endl;

  if( itk::Math::NotExactlyEquals(directionSwapDerivative[0], -origDerivative[1]) || itk::Math::NotExactlyEquals(directionSwapDerivative[1], origDerivative[0]) )
    {
    std::cerr << "ERROR: Expected origDerivative and directionSwapDerivative to be swapped." << std::endl;
    result = EXIT_FAILURE;
    }

  // test an out-of-bounds point with swapped dimensions
  // it should yield a deriviative of 0
  point[0] = 1.0;
  point[1] = 8.0;
  function->SetUseImageDirection( false );
  function->SetUseImageDirection( true );
  OutputType outOfBoundsDerivative = function->Evaluate( point );
  std::cout << "Point: " << point << " outOfBoundsDerivative: " << outOfBoundsDerivative << std::endl;

  if( itk::Math::NotExactlyEquals(outOfBoundsDerivative[0], 0) || itk::Math::NotExactlyEquals(outOfBoundsDerivative[1], 0) )
    {
    std::cerr << "ERROR: Expected derivative " << outOfBoundsDerivative[1] << ", 0 with out-of-bounds point." << std::endl;
    result = EXIT_FAILURE;
    }

  // another out-of-bounds point
  point[0] = -8.0;
  point[1] = 18.0;
  function->SetUseImageDirection( false );
  function->SetUseImageDirection( true );
  outOfBoundsDerivative = function->Evaluate( point );
  std::cout << "Point: " << point << " outOfBoundsDerivative: " << outOfBoundsDerivative << std::endl;

  if( itk::Math::NotExactlyEquals(outOfBoundsDerivative[0], 0) || itk::Math::NotExactlyEquals(outOfBoundsDerivative[1], 0) )
    {
    std::cerr << "ERROR: Expected derivative 0, " << outOfBoundsDerivative[0] << "with out-of-bounds point." << std::endl;
    result = EXIT_FAILURE;
    }

  if( result == EXIT_SUCCESS )
    {
    std::cout << "Test passed." << std::endl;
    }
  return result;

}
