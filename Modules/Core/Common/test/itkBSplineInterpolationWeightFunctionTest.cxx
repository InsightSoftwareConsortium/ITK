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

#include "itkBSplineInterpolationWeightFunction.h"

/*
 * This test exercises methods in the
 * BSplineInterpolationWeightFunction class.
 */
int itkBSplineInterpolationWeightFunctionTest(int, char* [] )
{

  { // Creating a local scope
  typedef double CoordRepType;
  const unsigned int SpaceDimension= 1;
  const unsigned int SplineOrder = 2;

  std::cout << "Testing SpaceDimension= " << SpaceDimension;
  std::cout << " and SplineOrder= " << SplineOrder << "  ";

  typedef itk::BSplineInterpolationWeightFunction<CoordRepType,
    SpaceDimension, SplineOrder>            FunctionType;
  typedef FunctionType::ContinuousIndexType ContinuousIndexType;
  typedef FunctionType::IndexType           IndexType;
  typedef FunctionType::WeightsType         WeightsType;

  FunctionType::Pointer function = FunctionType::New();

  WeightsType weights1;
  WeightsType weights2;

  weights1.SetSize( SplineOrder + 1 );
  weights2.SetSize( SplineOrder + 1 );

  ContinuousIndexType position1;
  ContinuousIndexType position2;

  IndexType startIndex1;
  IndexType startIndex2;

  double testFailed = false;

  for(double x=0.0; x<=2.0; x+=0.1 )
    {

    position1[0] =  x;
    position2[0] = -x;

    function->Evaluate( position1, weights1, startIndex1 );
    function->Evaluate( position2, weights2, startIndex2 );

    const unsigned int numberOfWeigts = weights1.size();

    const int indexDifference = itk::Math::abs( startIndex2[0] + startIndex1[0] ) & 1;


    const double tolerance = 1e-6;
    bool symmetryForXBroken = false;

    for( unsigned int nw = 0; nw < numberOfWeigts - indexDifference; nw++ )
      {
      if( itk::Math::abs( weights1[nw] - weights2[numberOfWeigts-nw-1-indexDifference] ) > tolerance )
        {
        symmetryForXBroken = true;
        }
      }

    if( symmetryForXBroken )
      {
      std::cerr << std::endl;
      std::cerr << "Error in weights symmetry for X = " << x << std::endl;
      testFailed = true;
      std::cerr << "indexDifference= " << indexDifference << std::endl;
      for( unsigned int nw = 0; nw < numberOfWeigts; nw++ )
        {
        std::cerr << weights1[nw] << "\t";
        }
      std::cerr << std::endl;
      for( unsigned int nw = 0; nw < numberOfWeigts; nw++ )
        {
        std::cerr << weights2[nw] << "\t";
        }
      std::cerr << std::endl;
      for( unsigned int sd = 0; sd < SpaceDimension; sd++ )
        {
        std::cerr << startIndex1[sd] << "\t";
        }
      std::cerr << std::endl;
      for( unsigned int sd = 0; sd < SpaceDimension; sd++ )
        {
        std::cerr << startIndex2[sd] << "\t";
        }
      std::cerr << std::endl;
      }
    }
  if( testFailed )
    {
    std::cerr << "Test Failed !" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed. " << std::endl;
  }
  { // Creating a local scope
  typedef double CoordRepType;
  const unsigned int SpaceDimension= 1;
  const unsigned int SplineOrder = 3;

  std::cout << "Testing SpaceDimension= " << SpaceDimension;
  std::cout << " and SplineOrder= " << SplineOrder << "  ";

  typedef itk::BSplineInterpolationWeightFunction<CoordRepType,
    SpaceDimension, SplineOrder>            FunctionType;
  typedef FunctionType::ContinuousIndexType ContinuousIndexType;
  typedef FunctionType::IndexType           IndexType;
  typedef FunctionType::WeightsType         WeightsType;

  FunctionType::Pointer function = FunctionType::New();

  WeightsType weights1;
  WeightsType weights2;

  weights1.SetSize( SplineOrder + 1 );
  weights2.SetSize( SplineOrder + 1 );

  ContinuousIndexType position1;
  ContinuousIndexType position2;

  IndexType startIndex1;
  IndexType startIndex2;

  double testFailed = false;

  for(double x=0.0; x<=2.0; x+=0.1 )
    {

    position1[0] =  x;
    position2[0] = -x;

    function->Evaluate( position1, weights1, startIndex1 );
    function->Evaluate( position2, weights2, startIndex2 );

    const unsigned int numberOfWeigts = weights1.size();

    const int indexDifference = itk::Math::abs( startIndex2[0] + startIndex1[0] + 1 ) & 1;


    const double tolerance = 1e-6;
    bool symmetryForXBroken = false;

    for( unsigned int nw = 0; nw < numberOfWeigts - indexDifference; nw++ )
      {
      if( itk::Math::abs( weights1[nw] - weights2[numberOfWeigts-nw-1-indexDifference] ) > tolerance )
        {
        symmetryForXBroken = true;
        }
      }

    if( symmetryForXBroken )
      {
      std::cerr << std::endl;
      std::cerr << "Error in weights symmetry for X = " << x << std::endl;
      testFailed = true;
      std::cerr << "indexDifference= " << indexDifference << std::endl;
      for( unsigned int nw = 0; nw < numberOfWeigts; nw++ )
        {
        std::cerr << weights1[nw] << "\t";
        }
      std::cerr << std::endl;
      for( unsigned int nw = 0; nw < numberOfWeigts; nw++ )
        {
        std::cerr << weights2[nw] << "\t";
        }
      std::cerr << std::endl;
      for( unsigned int sd = 0; sd < SpaceDimension; sd++ )
        {
        std::cerr << startIndex1[sd] << "\t";
        }
      std::cerr << std::endl;
      for( unsigned int sd = 0; sd < SpaceDimension; sd++ )
        {
        std::cerr << startIndex2[sd] << "\t";
        }
      std::cerr << std::endl;
      }
    }
  if( testFailed )
    {
    std::cerr << "Test Failed !" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "Test passed. " << std::endl;
  }

  { // Creating a local scope
  typedef double CoordRepType;
  const unsigned int SpaceDimension= 3;
  const unsigned int SplineOrder = 3;
  std::cout << "Testing SpaceDimension= " << SpaceDimension;
  std::cout << " and SplineOrder= " << SplineOrder << "  ";

  typedef itk::BSplineInterpolationWeightFunction<CoordRepType,
    SpaceDimension, SplineOrder>            FunctionType;
  typedef FunctionType::ContinuousIndexType ContinuousIndexType;
  typedef FunctionType::IndexType           IndexType;
  typedef FunctionType::WeightsType         WeightsType;
  typedef FunctionType::SizeType            SizeType;

  FunctionType::Pointer function = FunctionType::New();
  function->Print( std::cout );

  SizeType size = function->GetSupportSize();
  unsigned long numberOfWeights = function->GetNumberOfWeights();

  std::cout << "Number Of Weights: " << numberOfWeights << std::endl;

  ContinuousIndexType position;
  WeightsType weights;
  IndexType startIndex;

  position.Fill( 4.15 );
  weights = function->Evaluate( position );

  std::cout << "Position: " << position << std::endl;
  std::cout << "Weights: " << weights << std::endl;

  function->Evaluate( position, weights, startIndex );
  std::cout << "Position: " << position << std::endl;
  std::cout << "Weights: " << weights << std::endl;
  std::cout << "Start Index: " << startIndex << std::endl;


  // Check for accuracy
  typedef itk::BSplineKernelFunction<SplineOrder> KernelType;
  KernelType::Pointer kernel = KernelType::New();

  typedef itk::Image<char,SpaceDimension> ImageType;
  ImageType::Pointer image = ImageType::New();
  ImageType::RegionType region;
  region.SetIndex( startIndex );
  region.SetSize( size );

  image->SetRegions( region );
  image->Allocate(true); // initialize buffer to zero

  typedef itk::ImageRegionConstIteratorWithIndex<ImageType>
    IteratorType;
  IteratorType iter( image, image->GetBufferedRegion() );
  unsigned long counter = 0;

  while ( !iter.IsAtEnd() )
    {

    double value = 1.0;
    for ( unsigned int j = 0; j < SpaceDimension; j++ )
      {
      value *= kernel->Evaluate(
        static_cast<double>(iter.GetIndex()[j]) - position[j] );
      }
    if ( itk::Math::abs(weights[counter] - value) > 1e-7 )
      {
      std::cout << "Error at weights[" << counter << "]" << std::endl;
      std::cout << "Compuated value: " << weights[counter] << std::endl;
      std::cout << "Expected value: " << value  << std::endl;
      return EXIT_FAILURE;
      }

    ++counter;
    ++iter;
    }

  std::cout << "Test passed. " << std::endl;
  }
  return EXIT_SUCCESS;

}
