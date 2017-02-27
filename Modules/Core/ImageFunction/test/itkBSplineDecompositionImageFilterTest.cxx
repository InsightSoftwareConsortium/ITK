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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#include "itkFilterWatcher.h"
#include "vnl/vnl_sample.h"
#include "makeRandomImageBsplineInterpolator.h"
#include "itkMath.h"
#include "itkTestingMacros.h"


template< typename TFilter >
typename TFilter::SplinePolesVectorType ParseSplinePoles( char *splinePolesIn )
{
  typename TFilter::SplinePolesVectorType splinePolesOut;

  // Get the individual components
  char *endPtr;
  typename TFilter::SplinePolesVectorType::value_type value;
  while( *splinePolesIn )
    {
    value = strtod( splinePolesIn, &endPtr );
    if( splinePolesIn == endPtr )
      {
      (*splinePolesIn)++;
      }
    else if( endPtr == NULL || *endPtr == 0 )
      {
      splinePolesOut.push_back( value );
      break;
      }
    else
      {
      splinePolesOut.push_back( value );
      splinePolesIn = endPtr + 1;
      }
    }

  return splinePolesOut;
}


/** Note:  This is the same test used for the itkBSplineResampleImageFunctionTest
  *        It is duplicated here because it excercises the itkBSplineDecompositionFilter
  *        and demonstrates its use.
  */

int itkBSplineDecompositionImageFilterTest( int argc, char* argv[] )
{
  if( argc != 3 )
    {
    std::cerr << "Usage: " << argv[0];
    std::cerr << " splineOrder";
    std::cerr << " splinePoles(e.g. 0.12753,-0.48673,0.76439,\
                 [0.12753,-0.48673,0.76439],\
                 \"[0.12753 -0.48673 0.76439]\",\
                 \"(0.12753 -0.48673 0.76439)\", or\
                 \"0.12753 -0.48673 0.76439\")";
    std::cerr << std::endl;
    return EXIT_FAILURE;
    }

  const unsigned int ImageDimension = 2;
  typedef float                                   PixelType;
  typedef itk::Image< PixelType, ImageDimension > ImageType;
  typedef itk::BSplineInterpolateImageFunction< ImageType, double, double >
    BSplineInterpolatorFunctionType;

  unsigned int splineOrder = atoi( argv[1] );
  BSplineInterpolatorFunctionType::Pointer interpolator =
    makeRandomImageInterpolator< BSplineInterpolatorFunctionType >( splineOrder );
  ImageType::ConstPointer randImage = interpolator->GetInputImage();

  typedef itk::BSplineDecompositionImageFilter< ImageType, ImageType > FilterType;
  FilterType::Pointer filter = FilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, BSplineDecompositionImageFilter,
    ImageToImageFilter );

  FilterWatcher watcher( filter, "BSplineDecompositionImageFilter" );

  filter->SetInput( randImage );

  int unsupportedSplineOrder = 6;

  TRY_EXPECT_EXCEPTION( filter->SetSplineOrder( unsupportedSplineOrder ) );

  filter->SetSplineOrder( interpolator->GetSplineOrder() );

  FilterType::SplinePolesVectorType expectedSplinePoles = ParseSplinePoles< FilterType >( argv[2] );

  int expectedNumberOfPoles = expectedSplinePoles.size();
  int resultNumberOfPoles = filter->GetNumberOfPoles();
  if( !itk::Math::ExactlyEquals( expectedNumberOfPoles, resultNumberOfPoles ) )
    {
    std::cout << "Test failed!" << std::endl;
    std::cout << "Error in GetNumberOfPoles()" << std::endl;
    std::cout << "Expected: " << expectedNumberOfPoles << std::endl;
    std::cout << " , but got: " << resultNumberOfPoles << std::endl;
    return EXIT_FAILURE;
    }

  FilterType::SplinePolesVectorType resultSplinePoles = filter->GetSplinePoles();
  double tolerance1 = 1e-10;
  for( unsigned int i = 0; i < resultSplinePoles.size(); ++i )
    {
    FilterType::SplinePolesVectorType::value_type expectedSplinePole =
      expectedSplinePoles[i];
    FilterType::SplinePolesVectorType::value_type resultSplinePole =
      resultSplinePoles[i];
    if( !itk::Math::FloatAlmostEqual( expectedSplinePole, resultSplinePole, 10, tolerance1 ) )
      {
      std::cout.precision( static_cast< int >( itk::Math::abs( std::log10( tolerance1 ) ) ) );
      std::cout << "Test failed!" << std::endl;
      std::cout << "Error in GetSplinePoles() at index: [" << i << "]" << std::endl;
      std::cout << "Expected: " << expectedSplinePole << std::endl;
      std::cout << " , but got: " << resultSplinePole << std::endl;
      std::cout << " Values differ by more than: " << tolerance1 << std::endl;
      return EXIT_FAILURE;
      }
    }

  TRY_EXPECT_NO_EXCEPTION( filter->Update() );


  // Set up a BSplineResampleImageFunction.
  typedef itk::BSplineResampleImageFunction<ImageType,double> ResampleFunctionType;
  ResampleFunctionType::Pointer resample = ResampleFunctionType::New();

  resample->SetSplineOrder( interpolator->GetSplineOrder());
  resample->SetInputImage( filter->GetOutput() );

  // Compare 10 values at random points.

  ImageType::IndexType last;
  last.Fill( 0 );
  last[0] = randImage->GetLargestPossibleRegion().GetSize()[0] - 1;
  ImageType::PointType lastPhysicalLocation;
  randImage->TransformIndexToPhysicalPoint( last, lastPhysicalLocation);

  const double minValue = randImage->GetOrigin()[0];
  const double maxValue = lastPhysicalLocation[0];

  double tolerance2 = 1e-5;
  for( unsigned int k = 0; k < 10; ++k )
    {
    ResampleFunctionType::PointType point;
    for( unsigned int j = 0; j < ImageDimension; ++j )
      {
      point[j] = vnl_sample_uniform( minValue, maxValue );
      }

    const double f = resample->Evaluate( point );
    const double g = interpolator->Evaluate( point );

    if( !itk::Math::FloatAlmostEqual( f, g, 10, tolerance2 ) )
      {
      std::cout.precision( static_cast< int >( itk::Math::abs( std::log10( tolerance2 ) ) ) );
      std::cout << " Test failed! " << std::endl;
      std::cout << "Resample and Interpolated points are different." << std::endl;
      std::cout << " Point: " << point << std::endl;
      std::cout << " Resample: " << resample->Evaluate( point ) << std::endl;
      std::cout << " Interpolator: " << interpolator->Evaluate( point ) << std::endl;
      std::cout << " Values differ by more than: " << tolerance2 << std::endl;
      return EXIT_FAILURE;
      }
    }

  // Instantiation test with a std::complex pixel
  typedef std::complex< PixelType >                       ComplexPixelType;
  typedef itk::Image< ComplexPixelType, ImageDimension >  ComplexImageType;
  typedef itk::BSplineDecompositionImageFilter< ComplexImageType, ComplexImageType >
    ComplexFilterType;
  ComplexFilterType::Pointer complexFilter = ComplexFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( filter, BSplineDecompositionImageFilter,
    ImageToImageFilter );

  return EXIT_SUCCESS;
}
