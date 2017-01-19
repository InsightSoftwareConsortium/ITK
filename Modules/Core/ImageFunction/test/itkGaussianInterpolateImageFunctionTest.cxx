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

#include <iostream>

#include "itkImage.h"
#include "itkVectorImage.h"
#include "itkGaussianInterpolateImageFunction.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

// VS 2015 has a bug when building release with the heavily nested for
// loops iterating too many times. This turns off optimization to
// allow the tests to pass.
//
#if _MSC_VER == 1900
# pragma optimize( "", off )
#endif

// Allows testing up to TDimension = 4
template< unsigned int TDimension >
int RunTest( void )
{
  typedef float                                         PixelType;
  const   unsigned int                                  Dimensions = TDimension;

  typedef itk::Image<PixelType, TDimension>             ImageType;
  typedef typename ImageType::RegionType                RegionType;
  typedef typename RegionType::SizeType                 SizeType;
  typedef typename ImageType::IndexType                 IndexType;

  typedef float                                         CoordRepType;
  typedef typename itk::ContinuousIndex<CoordRepType, Dimensions>
                                                        ContinuousIndexType;
  typedef typename ContinuousIndexType::ValueType       AccumulatorType;
  typedef typename itk::Point<CoordRepType, Dimensions> PointType;

  typedef typename itk::GaussianInterpolateImageFunction<
                                                      ImageType,
                                                      CoordRepType >
                                                      InterpolatorType;

  typename ImageType::Pointer image = ImageType::New();

  IndexType start;
  start.Fill( 0 );

  SizeType size;
  const int dimMaxLength = 3;
  size.Fill( dimMaxLength );

  RegionType region;
  region.SetSize( size );
  region.SetIndex( start );

  image->SetRegions( region );
  image->Allocate();

  typename ImageType::PointType     origin;
  typename ImageType::SpacingType   spacing;

  origin.Fill( 0.0 );
  spacing.Fill( 1.0 );

  image->SetOrigin( origin );
  image->SetSpacing( spacing );

  image->Print( std::cout );

  // Setup for testing up to Dimension = 4
  unsigned int dimLengths[4] = {1,1,1,1};
  for( unsigned int ind = 0; ind < Dimensions; ind++ )
    {
    dimLengths[ind] = dimMaxLength;
    }

  //
  // Fill up the image values with the function
  //
  // Intensity = f(d1[,d2[,d3[,d4]]]) = 3*d1 [+ d2 [+ d3 [+ d4] ] ]
  //
  IndexType index;
  PixelType value;
  unsigned int dimIt[4];
  std::cout << "Image Data: " << std::endl;
  for (dimIt[3] = 0; dimIt[3] < dimLengths[3]; dimIt[3]++)
    {
    for (dimIt[2] = 0; dimIt[2] < dimLengths[2]; dimIt[2]++)
      {
      std::cout << "* dimIt[3], dimIt[2]: " << dimIt[3] << ", " << dimIt[2]
                << std::endl;
      for (dimIt[1] = 0; dimIt[1] < dimLengths[1]; dimIt[1]++)
        {
        for (dimIt[0] = 0; dimIt[0] < dimLengths[0]; dimIt[0]++)
          {
          value = 3*dimIt[0];
          index[0] = dimIt[0];
          for( unsigned int ind = 1; ind < Dimensions; ind++ )
          {
          value += dimIt[ind];
          index[ind] = dimIt[ind];
          }
          image->SetPixel( index, value );

          std::cout << value << " ";
          }
        std::cout << std::endl;
        }
      }
    }

  typename InterpolatorType::Pointer interpolator = InterpolatorType::New();

  interpolator->SetInputImage( image );

  double sigma[TDimension];
  for( unsigned int d = 0; d < TDimension; d++ )
    {
    sigma[d] = 1.0;
    }
  AccumulatorType alpha = 1.0;

  interpolator->SetParameters( sigma, alpha );

  for( unsigned int d = 0; d < TDimension; d++ )
    {
    TEST_SET_GET_VALUE( sigma[d], interpolator->GetSigma()[d] );
    }

  TEST_SET_GET_VALUE( alpha, interpolator->GetAlpha() );

  // Test EvaluateAtContinuousIndex
  // Use the last index for convenience
  typename InterpolatorType::ContinuousIndexType cindex = index;
  if ( interpolator->IsInsideBuffer( index ) )
    {
    typename InterpolatorType::OutputType interpolatedValue;
    interpolatedValue = interpolator->EvaluateAtContinuousIndex(cindex);

    itk::Math::FloatAlmostEqual( (float)interpolatedValue, (float)value );
    }

  const AccumulatorType incr = 0.2;

  const AccumulatorType tolerance = 5e-6;

  PointType point;
  AccumulatorType testLengths[4] = {1,1,1,1};
  for( unsigned int ind = 0; ind < Dimensions; ind++ )
  {
  testLengths[ind] = dimMaxLength-1;
  }
  AccumulatorType steps[4];
  AccumulatorType dimItf[4];
  for (dimItf[3] = 0; dimItf[3] < testLengths[3]; dimItf[3]++)
    {
    for (dimItf[2] = 0; dimItf[2] < testLengths[2]; dimItf[2]++)
      {
      for (dimItf[1] = 0; dimItf[1] < testLengths[1]; dimItf[1]++)
        {
        for (dimItf[0] = 0; dimItf[0] < testLengths[0]; dimItf[0]++)
          {
          for (steps[3] = 0; steps[3] < dimItf[3] + 1.01; steps[3]+=incr)
            {
            for (steps[2] = 0; steps[2] < dimItf[2] + 1.01; steps[2]+=incr)
              {
              for (steps[1] = 0; steps[1] < dimItf[1] + 1.01; steps[1]+=incr)
                {
                for (steps[0] = 0; steps[0] < dimItf[0] + 1.01; steps[0]+=incr)
                  {
                  AccumulatorType expectedValue = 3*steps[0];
                  point[0] = steps[0];
                  for( unsigned int ind = 1; ind < Dimensions; ind++ )
                  {
                  expectedValue += steps[ind];
                  point[ind]=steps[ind];
                  }

                  if( interpolator->IsInsideBuffer( point ) )
                    {
                    const AccumulatorType computedValue = interpolator->Evaluate( point );

                    if( ! itk::Math::FloatAlmostEqual( expectedValue, computedValue, 7, tolerance ) )
                      {
                      return EXIT_FAILURE;
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  return EXIT_SUCCESS;
}

int itkGaussianInterpolateImageFunctionTest( int, char*[] )
{
  typedef float                                         PixelType;
  const   unsigned int                                  Dimension = 1;

  typedef itk::Image< PixelType, Dimension >            ImageType;
  typedef float                                         CoordRepType;

  typedef itk::GaussianInterpolateImageFunction< ImageType,
    CoordRepType > InterpolatorType;

  InterpolatorType::Pointer interpolator = InterpolatorType::New();

  EXERCISE_BASIC_OBJECT_METHODS( interpolator, GaussianInterpolateImageFunction,
    InterpolateImageFunction );


  // Test separately for images of 1 through 4 dimensions because this function
  // has optimized implementations for dimensionality of 1-3, and unoptimized
  // implementation for 4 and greater.
  int testResult = EXIT_SUCCESS;
  int testResult1 = EXIT_SUCCESS;
  int testResult2 = EXIT_SUCCESS;
  int testResult3 = EXIT_SUCCESS;
  int testResult4 = EXIT_SUCCESS;

  std::cout << "***** Testing dimensionality of 1 *****" << std::endl;
  if( RunTest<1>() == EXIT_FAILURE )
    {
    testResult1 = EXIT_FAILURE;
    std::cout << "Failed for dimensionality 1." << std::endl;
    }
  std::cout << "***** Testing dimensionality of 2 *****" << std::endl;
  if( RunTest<2>() == EXIT_FAILURE )
    {
    testResult2 = EXIT_FAILURE;
    std::cout << "Failed for dimensionality 2." << std::endl;
    }
  std::cout << "***** Testing dimensionality of 3 *****" << std::endl;
  if( RunTest<3>() == EXIT_FAILURE )
    {
    testResult3 = EXIT_FAILURE;
    std::cout << "Failed for dimensionality 3." << std::endl;
    }
  std::cout << "***** Testing dimensionality of 4 *****" << std::endl;
  if( RunTest<4>() == EXIT_FAILURE )
    {
    testResult4 = EXIT_FAILURE;
    std::cout << "Failed for dimensionality 4." << std::endl;
    }

  if( testResult1 == EXIT_SUCCESS && testResult2 == EXIT_SUCCESS &&
    testResult3 == EXIT_SUCCESS && testResult4 == EXIT_SUCCESS )
    {
    std::cout << "TEST FINISHED SUCCESSFULLY!" << std::endl;
    }
  else
    {
    std::cout << "TEST FAILED!" << std::endl;
    testResult = EXIT_FAILURE;
    }

  return testResult;
 }
