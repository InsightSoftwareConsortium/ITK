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

#include "itkMath.h"
#include "itkCentralDifferenceImageFunction.h"
#include "itkImageRegionIterator.h"
#include "itkTestingMacros.h"

template< typename T >
bool IsEqual( T & m1, T & m2 )
{
  for( unsigned int r=0; r < T::RowDimensions; r++ )
    {
    for( unsigned int c=0; c < T::ColumnDimensions; c++ )
      {
      if( std::fabs( m1(r,c) - m2(r,c) ) > 1e-4 )
        {
        return false;
        }
      }
    }
  return true;
}

template< unsigned int VectorLength >
int itkCentralDifferenceImageFunctionOnVectorTestRun( )
{
  std::cout << "\n**************************" << std::endl
            << "VectorLength: " << VectorLength << std::endl << std::endl;

  int result = EXIT_SUCCESS;

  const unsigned int                            ImageDimension = 2;
  typedef itk::Vector<float,VectorLength>       PixelType;
  typedef itk::Image<PixelType,ImageDimension>  ImageType;

  typename ImageType::Pointer image = ImageType::New();
  typename ImageType::SizeType size;
  size.Fill( 16 );
  typename ImageType::RegionType region( size );

  image->SetRegions( region );
  image->Allocate();

  // make a test image
  typedef itk::ImageRegionIterator<ImageType> Iterator;
  Iterator iter( image, region );
  iter.GoToBegin();
  unsigned int counter = 0;

  while ( !iter.IsAtEnd() )
    {
    PixelType pix;
    pix[0] = counter * counter;
    for( unsigned int i=1; i < VectorLength; i++ )
      {
      pix[i] = pix[i-1] / 10.0;
      }
    iter.Set( pix );
    ++counter;
    ++iter;
    }

  // set up central difference calculator
  typedef float                                           CoordRepType;
  typedef itk::Matrix<double,VectorLength,ImageDimension> DerivativeType;

  typedef itk::CentralDifferenceImageFunction<ImageType,CoordRepType,DerivativeType>  FunctionType;
  typedef typename FunctionType::OutputType                                           OutputType;
  typedef typename FunctionType::OutputValueType                                      OutputValueType;

  typename FunctionType::Pointer function = FunctionType::New();

  function->SetInputImage( image );

  typename ImageType::IndexType index;

  // pick an index inside the image
  index.Fill( 8 );
  OutputType indexOutput = function->EvaluateAtIndex( index );
  std::cout << "Index: " << index << " Derivative: ";
  std::cout << indexOutput << std::endl;

  // verify the output
  OutputType truthOutput;
  for( unsigned int dim = 0; dim < ImageDimension; dim++ )
    {
    PixelType deriv;
    typename ImageType::IndexType indexTest = index;
    indexTest[dim] = indexTest[dim] + 1;
    deriv = image->GetPixel( indexTest );
    indexTest[dim] = indexTest[dim] - 2;
    deriv -= image->GetPixel( indexTest );
    deriv /= 2.0;
    for( unsigned int nc = 0; nc < VectorLength; nc++ )
      {
      truthOutput[nc][dim] = deriv[nc];
      }
    }

  if( ! IsEqual<OutputType>( indexOutput, truthOutput) )
    {
    std::cout << "ERROR: indexOutput " << indexOutput << " does not match truth: " << truthOutput << std::endl;
    result = EXIT_FAILURE;
    }

  if ( function->IsInsideBuffer( index ) )
    {
    std::cout << "Index: " << index << " is inside the BufferedRegion." << std::endl;
    }
  else
    {
    std::cout << "Expected index " << index << " to be inside BufferedRegion. " << std::endl;
    result = EXIT_FAILURE;
    }

  // test continuous index
  typename FunctionType::ContinuousIndexType cindex;
  cindex.Fill( 8.0 );
  OutputType continuousIndexOutput = function->EvaluateAtContinuousIndex( cindex );
  std::cout << "ContinuousIndex: " << cindex << " Derivative: ";
  std::cout << continuousIndexOutput << std::endl;

  if( ! IsEqual<OutputType>( indexOutput, continuousIndexOutput ) )
    {
    std::cout << "ERROR: Output of EvaluateAtIndex and EvaluateAtContinuousIndex do not match." << std::endl;
    result = EXIT_FAILURE;
    }

  typename FunctionType::PointType point;
  point.Fill( 8.0 );
  OutputType pointOutput = function->Evaluate( point );
  std::cout << "Point: " << point << " Derivative: ";
  std::cout << pointOutput << std::endl;

  // this should be the same as output from EvaluateAtIndex as long as
  // image is setup with default spatial information.
  if( ! IsEqual<OutputType>( indexOutput, pointOutput ) )
    {
    std::cout << "ERROR: Output of EvaluateAtIndex and Evaluate do not match." << std::endl;
    std::cout << "difference: " << indexOutput - pointOutput << std::endl;
    result = EXIT_FAILURE;
    }

  // test on the image edge. expect derivative in that dimension to be zero.
  index.Fill( 8 );
  index[0] = 15;
  indexOutput = function->EvaluateAtIndex( index );
  std::cout << "Index: " << index << " Derivative: ";
  std::cout << indexOutput << std::endl;

  if ( function->IsInsideBuffer( index ) )
    {
    std::cout << "Index: " << index << " is inside the BufferedRegion." << std::endl;
    }
  for( itk::SizeValueType n=0; n < VectorLength; n++ )
    {
    if( itk::Math::NotAlmostEquals(indexOutput(n,0), itk::NumericTraits<OutputValueType>::ZeroValue()) )
      {
      std::cout << "ERROR: Index: " << index << " expected output dim 0 to be 0. << std::endl; " << std::endl;
      result = EXIT_FAILURE;
      break;
      }
    }

  cindex.Fill( 8.0 );
  cindex[0] = 15.0;
  continuousIndexOutput = function->EvaluateAtContinuousIndex( cindex );
  std::cout << "ContinuousIndex: " << cindex << " Derivative: ";
  std::cout << continuousIndexOutput << std::endl;

  if( ! IsEqual<OutputType>( indexOutput, continuousIndexOutput ) )
    {
    std::cout << "ERROR: Output of EvaluateAtIndex and EvaluateAtContinuousIndex "
              << "do not match at boundary." << std::endl;
    result = EXIT_FAILURE;
    }

  point.Fill( 8.0 );
  point[0] = 15.0;
  pointOutput = function->Evaluate( point );
  std::cout << "Point: " << point << " Derivative: ";
  std::cout << pointOutput << std::endl;

  if( ! IsEqual<OutputType>( indexOutput, pointOutput ) )
    {
    std::cout << "ERROR: Output of EvaluateAtIndex and EvaluateAtContinuousIndex "
              << "do not match at boundary." << std::endl;
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
  for( itk::SizeValueType n=0; n < VectorLength; n++ )
    {
    if( itk::Math::NotAlmostEquals(indexOutput(n,1), itk::NumericTraits<OutputValueType>::ZeroValue()) )
      {
      std::cout << "ERROR: Index: " << index << " expected output dim 1 to be 0. " << std::endl;
      result = EXIT_FAILURE;
      }
    }

  cindex.Fill( 8.0 );
  cindex[1] = 0;
  continuousIndexOutput = function->EvaluateAtContinuousIndex( cindex );
  std::cout << "ContinuousIndex: " << cindex << " Derivative: ";
  std::cout << continuousIndexOutput << std::endl;

  if( ! IsEqual<OutputType>( indexOutput, continuousIndexOutput ) )
    {
    std::cout << "ERROR: Output of EvaluateAtIndex and EvaluateAtContinuousIndex "
              << "do not match at boundary." << std::endl;
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

  if( ! IsEqual<OutputType>( indexOutput, pointOutput ) )
    {
    std::cout << "ERROR: Output of EvaluateAtIndex and EvaluateAtContinuousIndex "
              << "do not match at boundary." << std::endl;
    result = EXIT_FAILURE;
    }

  // DO NOT test out-of-bounds index or point.
  // Method documentation states that index/point is assumed
  // to be in bounds.

  // test results at non-interger positions
  std::cout << "Test non-integer position for EvaluateAtContinuousIndex. " << std::endl;
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
    std::cout << "ERROR: Failed for EvaluateAtContinuousIndex at non-interger indecies. "
              << "Results are unexpectedly identical." << std::endl;
    result = EXIT_FAILURE;
    }

  if( fabs( ( right[0][0] + left[0][0] ) / 2.0 - center[0][0] ) > 1e-06 )
    {
    std::cout << "ERROR: Failed for EvaluateAtContinuousIndex at non-integer incecies. Center index result is not average of left and right." << std::endl;
    result = EXIT_FAILURE;
    }

  std::cout << "Test non-integer position for Evaluate. " << std::endl;
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
    std::cout << "ERROR: Failed for Evaluate at non-interger indecies. Results are unexpectedly identical." << std::endl;
    result = EXIT_FAILURE;
    }

  if( fabs( ( right[0][0] + left[0][0] ) / 2.0 - center[0][0] ) > 1e-06 )
    {
    std::cout << "ERROR: Failed for Evaluate at non-integer incecies. Center index result is not average of left and right." << std::endl;
    result = EXIT_FAILURE;
    }

  // test image direction and Evaluate
  point.Fill( 8.0 );
  OutputType origDerivative = function->Evaluate( point );
  std::cout << "Point: " << point << " origDerivative: " << origDerivative << std::endl;

  typename ImageType::DirectionType direction;
  direction[0][0] = -1.0;
  direction[0][1] = 0.0;
  direction[1][0] = 0.0;
  direction[1][1] = -1.0;
  point.Fill( -8.0 );
  image->SetDirection( direction );
  function->SetUseImageDirection( true );
  OutputType directionOnDerivative = function->Evaluate( point );
  std::cout << "Point: " << point << " directionOnDerivative: " << directionOnDerivative << std::endl;

  if( itk::Math::NotAlmostEquals(directionOnDerivative[0][0], -origDerivative[0][0]) ||
      itk::Math::NotAlmostEquals(directionOnDerivative[0][1], -origDerivative[0][1]) )
    {
    std::cout << "ERROR: Expected origDerivative and directionOnDerivative to be opposite." << std::endl;
    result = EXIT_FAILURE;
    }

  // with image direction disabled, result should be same as with
  // identity direction
  function->SetUseImageDirection( false );
  OutputType directionOffDerivative = function->Evaluate( point );
  std::cout << "Point: " << point << " directionOffDerivative: " << directionOffDerivative << std::endl;

  if( ! IsEqual<OutputType>( directionOffDerivative, origDerivative ) )
    {
    std::cout << "Expected origDerivative == directionOffDerivative." << std::endl;
    result = EXIT_FAILURE;
    }

  // Test with incorrectly-sized output type
  typedef itk::Matrix<double,10,ImageDimension> BadDerivativeType;

  typedef itk::CentralDifferenceImageFunction<ImageType,CoordRepType,BadDerivativeType>  BadFunctionType;

  typename BadFunctionType::Pointer badFunction = BadFunctionType::New();
  TRY_EXPECT_EXCEPTION( badFunction->SetInputImage( image ) );

  return result;
}

/////////////////////////////////////////////////////////////////

int itkCentralDifferenceImageFunctionOnVectorTest(int, char* [] )
{

  if( itkCentralDifferenceImageFunctionOnVectorTestRun<1>() == EXIT_FAILURE )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  if( itkCentralDifferenceImageFunctionOnVectorTestRun<2>() == EXIT_FAILURE )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  if( itkCentralDifferenceImageFunctionOnVectorTestRun<3>() == EXIT_FAILURE )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  if( itkCentralDifferenceImageFunctionOnVectorTestRun<4>() == EXIT_FAILURE )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  if( itkCentralDifferenceImageFunctionOnVectorTestRun<5>() == EXIT_FAILURE )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }
  if( itkCentralDifferenceImageFunctionOnVectorTestRun<6>() == EXIT_FAILURE )
    {
    std::cout << "FAILED." << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << std::endl << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
