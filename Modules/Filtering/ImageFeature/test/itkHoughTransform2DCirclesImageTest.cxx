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

#include "itkHoughTransform2DCirclesImageFilter.h"
#include "itkThresholdImageFilter.h"
#include "itkGradientMagnitudeImageFilter.h"
#include "itkCastImageFilter.h"
#include "itkMath.h"
#include "itkTestingMacros.h"


// Define the dimension of the images
static const unsigned int Dimension = 2;


template< typename ImageType >
void CreateCircle( typename ImageType::Pointer image, unsigned int center[Dimension], double radius )
{
  typename ImageType::IndexType index = image->GetLargestPossibleRegion().GetIndex();

  for( double i = 0; i <= radius; i += 0.1 )
  {
    for( double angle = 0; angle <= 2 * itk::Math::pi; angle += itk::Math::pi / 1000 )
    {
      index[0] = (long int)( center[0] + i * std::cos( angle ) );
      index[1] = (long int)( center[1] + i * std::sin( angle ) );
      image->SetPixel( index, 255 );
    }
  }
}


int itkHoughTransform2DCirclesImageTest( int, char* [] )
{

  // Declare the pixel types of the images
  typedef unsigned char                           PixelType;
  typedef double                                  HoughSpacePixelType;

  // Declare the types of the images
  typedef itk::Image< HoughSpacePixelType, Dimension >      HoughImageType;
  typedef itk::Image< PixelType, Dimension >                ImageType;

  // Create a black image
  ImageType::Pointer image = ImageType::New();

  ImageType::RegionType region;

  ImageType::SizeType size;
  size.Fill( 100 );

  ImageType::IndexType index;
  index.Fill( 0 );

  region.SetSize( size );
  region.SetIndex( index );

  image->SetRegions( region );
  image->Allocate( true ); // initialize buffer to zero

  // Create 3 circles
  const unsigned int circles = 3;

  unsigned int center[circles][Dimension];
  double radius[circles];

  center[0][0] = 50;
  center[0][1] = 50;
  radius[0] = 15;

  center[1][0] = 25;
  center[1][1] = 25;
  radius[1] = 7;

  center[2][0] = 71;
  center[2][1] = 72;
  radius[2] = 5;

  for( unsigned int i = 0; i < circles; ++i )
    {
    CreateCircle< ImageType >( image, center[i], radius[i] );
    }

  // Allocate Hough Space image (accumulator)
  ImageType::Pointer m_HoughSpaceImage = ImageType::New();
  m_HoughSpaceImage->SetRegions( region );
  m_HoughSpaceImage->Allocate( true ); // initialize buffer to zero

  // Apply gradient filter to the input image
  typedef itk::CastImageFilter< ImageType, HoughImageType > CastingFilterType;

  CastingFilterType::Pointer caster = CastingFilterType::New();
  caster->SetInput( image );

  typedef itk::GradientMagnitudeImageFilter< HoughImageType, HoughImageType >
    GradientFilterType;

  GradientFilterType::Pointer gradFilter = GradientFilterType::New();
  gradFilter->SetInput( caster->GetOutput() );

  gradFilter->Update();

  // Apply a threshold to the Grad(InputImage)
  typedef itk::ThresholdImageFilter< HoughImageType > ThresholdFilterType;

  ThresholdFilterType::Pointer threshFilter = ThresholdFilterType::New();
  threshFilter->SetInput( gradFilter->GetOutput() );
  threshFilter->SetOutsideValue( 0 );
  unsigned char lowerThreshold = 10;
  unsigned char upperThreshold = 255;
  threshFilter->ThresholdOutside( lowerThreshold, upperThreshold );

  threshFilter->Update();

  // Define the HoughTransform filter
  typedef itk::HoughTransform2DCirclesImageFilter< HoughSpacePixelType,
    HoughSpacePixelType > HoughTransformFilterType;

  HoughTransformFilterType::Pointer houghFilter = HoughTransformFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( houghFilter, HoughTransform2DCirclesImageFilter,
    ImageToImageFilter );


  double threshold = 2.3;
  houghFilter->SetThreshold( threshold );
  TEST_SET_GET_VALUE( threshold, houghFilter->GetThreshold() );

  double minMaxRadius = 16.2;
  houghFilter->SetRadius( minMaxRadius );
  TEST_SET_GET_VALUE( minMaxRadius, houghFilter->GetMinimumRadius() );
  TEST_SET_GET_VALUE( minMaxRadius, houghFilter->GetMaximumRadius() );

  double minimumRadius = 2.1;
  houghFilter->SetMinimumRadius( minimumRadius );
  TEST_SET_GET_VALUE( minimumRadius, houghFilter->GetMinimumRadius() );

  double maximumRadius = 20.4;
  houghFilter->SetMaximumRadius( maximumRadius );
  TEST_SET_GET_VALUE( maximumRadius, houghFilter->GetMaximumRadius() );

  double sigmaGradient = 1.2;
  houghFilter->SetSigmaGradient( sigmaGradient );
  TEST_SET_GET_VALUE( sigmaGradient, houghFilter->GetSigmaGradient() );

  float discRadiusRatio = 1.1;
  houghFilter->SetDiscRadiusRatio( discRadiusRatio );
  TEST_SET_GET_VALUE( discRadiusRatio, houghFilter->GetDiscRadiusRatio() );

  float variance = 10;
  houghFilter->SetVariance( variance );
  TEST_SET_GET_VALUE( variance, houghFilter->GetVariance() );

  float sweepAngle = 0.2;
  houghFilter->SetSweepAngle( sweepAngle );
  TEST_SET_GET_VALUE( sweepAngle, houghFilter->GetSweepAngle() );

  HoughTransformFilterType::CirclesListSizeType numberOfCircles =
    static_cast< HoughTransformFilterType::CirclesListSizeType >( circles );
  houghFilter->SetNumberOfCircles( numberOfCircles );
  TEST_SET_GET_VALUE( numberOfCircles, houghFilter->GetNumberOfCircles() );

  houghFilter->SetInput( threshFilter->GetOutput() );

  TRY_EXPECT_EXCEPTION( houghFilter->GetCircles() );

  houghFilter->Update();

  // Check the circle radius
  HoughTransformFilterType::CirclesListType circleList =
    houghFilter->GetCircles();

  circleList = houghFilter->GetCircles( circles );

  double radiusTolerance = 2.0;

  HoughTransformFilterType::CirclesListType::const_iterator it =
    circleList.begin();

  unsigned int i = 0;
  while( it != circleList.end() )
    {
      if( !itk::Math::FloatAlmostEqual( (double)( it->GetPointer()->GetRadius()[0] ),
        radius[i], 10, radiusTolerance ) &&
        !itk::Math::FloatAlmostEqual( (double)( it->GetPointer()->GetRadius()[0] ),
        radius[i] * discRadiusRatio, 10, radiusTolerance ) )
      {
      std::cout << "Failure for circle #" << i << std::endl;
      std::cout << "Expected radius: " << radius[i] << ", found " << it->GetPointer()->GetRadius() << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cout << "Circle #" << i << " radius: " << it->GetPointer()->GetRadius() << std::endl;
      }
    ++it;
    ++i;
    }

  // Check the circle center
  HoughImageType::Pointer accumulator = houghFilter->GetOutput();

  HoughImageType::ConstPointer radiusImage = houghFilter->GetRadiusImage();

  // Blur the accumulator in order to find the maximum
  typedef itk::DiscreteGaussianImageFilter< HoughImageType, HoughImageType >
    GaussianFilterType;

  GaussianFilterType::Pointer gaussianFilter = GaussianFilterType::New();
  gaussianFilter->SetInput( accumulator );
  double gaussianFilterVariance[Dimension];
  gaussianFilterVariance[0] = variance;
  gaussianFilterVariance[1] = variance;
  gaussianFilter->SetVariance( gaussianFilterVariance );
  gaussianFilter->SetMaximumError( .01f );

  gaussianFilter->Update();

  HoughImageType::Pointer postProcessImage = gaussianFilter->GetOutput();

  typedef itk::MinimumMaximumImageCalculator< HoughImageType > MinMaxCalculatorType;
  MinMaxCalculatorType::Pointer minMaxCalculator = MinMaxCalculatorType::New();

  itk::ImageRegionIterator< ImageType > it_output( m_HoughSpaceImage,
    m_HoughSpaceImage->GetLargestPossibleRegion() );

  itk::ImageRegionIterator< HoughImageType > it_input( postProcessImage,
    postProcessImage->GetLargestPossibleRegion() );


  // Search for maxima
  unsigned int centerResult[circles][Dimension];
  double radiusResult[circles];
  unsigned int foundCircles = 0;
  do
    {
    minMaxCalculator->SetImage( postProcessImage );
    minMaxCalculator->ComputeMaximum();

    HoughImageType::PixelType max = minMaxCalculator->GetMaximum();

    it_output.GoToBegin();
    for( it_input.GoToBegin(); !it_input.IsAtEnd(); ++it_input )
    {
      if( itk::Math::ExactlyEquals( it_input.Get(), max ) )
      {
        it_output.Set( 255 );
        double radius2 = radiusImage->GetPixel( it_output.GetIndex() );
        centerResult[foundCircles][0] = it_output.GetIndex()[0];
        centerResult[foundCircles][1] = it_output.GetIndex()[1];
        radiusResult[foundCircles] = radius2;

        // Draw the circle
        for( double angle = 0; angle <= 2 * itk::Math::pi; angle += itk::Math::pi / 1000 )
        {
          index[0] = (long int)( it_output.GetIndex()[0] + radius2 * std::cos( angle ) );
          index[1] = (long int)( it_output.GetIndex()[1] + radius2 * std::sin( angle ) );
          m_HoughSpaceImage->SetPixel( index, 255 );

          // Remove the maximum from the accumulator
          for( double length = 0; length < discRadiusRatio * radius2; length += 1 )
          {
            index[0] = (long int)( it_output.GetIndex()[0] + length * std::cos( angle ) );
            index[1] = (long int)( it_output.GetIndex()[1] + length * std::sin( angle ) );
            postProcessImage->SetPixel( index, 0 );
          }
        }

        minMaxCalculator->SetImage( postProcessImage );
        minMaxCalculator->ComputeMaximum();
        max = minMaxCalculator->GetMaximum();

        foundCircles++;
        if( foundCircles == numberOfCircles )
          {
          break;
          }
      }
      ++it_output;
      }
    }
  while( foundCircles < numberOfCircles );

  // Check the circle detection
  double centerTolerance = 2.0;
  for( i = 0; i < numberOfCircles; i++ )
    {
      if( !itk::Math::FloatAlmostEqual( (double)( centerResult[i][0] ), (double)( center[i][0] ), 10, centerTolerance ) ||
       !itk::Math::FloatAlmostEqual( (double)( centerResult[i][1] ), (double)( center[i][1] ), 10, centerTolerance ) ||
       ( !itk::Math::FloatAlmostEqual( radiusResult[i], radius[i], 10, radiusTolerance ) &&
        !itk::Math::FloatAlmostEqual( radiusResult[i], radius[i] * discRadiusRatio, 10, radiusTolerance ) ) )
      {
      std::cout << "Failure for circle #" << i << std::endl;
      std::cout << "Expected center: [" << center[i][0] << ", " << center[i][1]
                << "], found [" << centerResult[i][0] << ", " << centerResult[i][1] << "]" << std::endl;
      std::cout << "Excpected radius: " << radius[i] << ", found " << radiusResult[i] << std::endl;
      return EXIT_FAILURE;
      }
    else
      {
      std::cout << "Circle #" << i << " [" << centerResult[i][0] << ", "
                << centerResult[i][1] << "] -> radius: " << radiusResult[i] << std::endl;
      }
    }

  std::cout << "Test succeeded!" << std::endl;
  return EXIT_SUCCESS;
}
