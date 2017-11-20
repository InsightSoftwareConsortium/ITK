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

#include "itkHoughTransform2DLinesImageFilter.h"
#include "itkThresholdImageFilter.h"
#include "itkGradientMagnitudeImageFilter.h"
#include "itkMath.h"
#include "itkTestingMacros.h"

/**
 * This program looks for straight lines whithin an image
 * It uses the ITK HoughTransform2DLinesImageFilter.
 * - Read the image.
 * - Apply a gradient and thresholding functions.
 * - Compute the accumulator by running the filter.
 * - Blur the accumulator.
 * - Find maxima in the accumulator.
 * - Display the results
 *
 * It also does unit testing.
 */

namespace
{
  bool Test_GetLines_should_return_empty_list_when_input_image_is_entirely_black()
  {
    typedef unsigned char PixelType;

    typedef itk::Image<PixelType> ImageType;

    typedef itk::HoughTransform2DLinesImageFilter< PixelType, double > FilterType;

    // Create a black input image for the filter.
    const ImageType::Pointer image = ImageType::New();
    const ImageType::SizeType size = { { 32, 32 } };
    image->SetRegions(size);
    image->Allocate(true);

    const FilterType::Pointer filter = FilterType::New();
    filter->SetInput(image);
    filter->Update();

    if (!filter->GetLines().empty())
    {
      std::cout << "GetLines() should return an empty list when the input image is entirely black."
        << std::endl;
      return false;
    }
    return true;
  }


  bool Test_GetLines_should_return_empty_list_when_NumberOfLines_is_set_to_zero()
  {
    typedef unsigned char PixelType;

    typedef itk::Image<PixelType> ImageType;

    // Create an image.
    const ImageType::Pointer image = ImageType::New();
    enum { sizeX = 32, sizeY = 32 };
    const ImageType::SizeType size = { { sizeX, sizeY } };
    image->SetRegions(size);
    image->Allocate(true);

    // Place some line segment in the image.
    for ( itk::IndexValueType x = 1; x < (sizeX - 1); ++x )
    {
      const itk::Index<> index = { {x, sizeY / 2} };
      image->SetPixel(index, 1);
    }

    typedef itk::HoughTransform2DLinesImageFilter< PixelType, double > FilterType;

    const FilterType::Pointer filter = FilterType::New();

    filter->SetInput(image);
    filter->SetNumberOfLines(0);
    filter->Update();

    // Even when there appears a line segment in the image, GetLines() should return an empty list
    // because SetNumberOfLines(0) was called.
    if (!filter->GetLines().empty())
    {
      std::cout << "GetLines() should return an empty list when NumberOfLines is set to zero."
        << std::endl;
      return false;
    }
    return true;
  }
}


/** Hough Point structure */
struct HoughPoint
{
  double radius;
  double angle;
};


int itkHoughTransform2DLinesImageTest( int, char* [] )
{
  bool success = true;

  // Define the dimension of the images
  const unsigned Dimension = 2;

  // Declare the pixel types of the images
  typedef unsigned char                           PixelType;
  typedef double                                  HoughSpacePixelType;

  // Declare the types of the images
  typedef itk::Image< HoughSpacePixelType, Dimension >      HoughImageType;
  typedef itk::Image< PixelType, Dimension >                ImageType;


  // Create a line image with one line
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

  // Create a line
  const unsigned int lines = 1;
  double theta = 0.20; // radians
  double radius = 50;

  double Vx = radius * std::cos( theta );
  double Vy = radius * std::sin( theta );

  double norm = std::sqrt( Vx * Vx + Vy * Vy );
  double VxNorm = Vx / norm;
  double VyNorm = Vy / norm;

  unsigned int numberOfPixels = size[0] * size[1];

  for( unsigned int i = 0; i < numberOfPixels; i += 1 )
  {
    index[0] = (long int)( Vx - VyNorm * i );
    index[1] = (long int)( Vy + VxNorm * i );

    if( index[0] < (long)size[0] && index[0] >= 0
         && index[1] < (long)size[1] && index[1] >= 0 )
    {
       image->SetPixel( index, 255 );
    }
  }

  // Allocate Hough Space image (accumulator)
  HoughImageType::Pointer m_HoughSpaceImage = HoughImageType::New();
  m_HoughSpaceImage->SetRegions( region );
  m_HoughSpaceImage->Allocate();

  // Apply gradient filter to the input image
 typedef itk::CastImageFilter< ImageType, HoughImageType > CastingFilterType;

  CastingFilterType::Pointer caster = CastingFilterType::New();
  caster->SetInput( image );


  typedef itk::GradientMagnitudeImageFilter< HoughImageType, HoughImageType >
    GradientFilterType;

  GradientFilterType::Pointer gradFilter = GradientFilterType::New();
  gradFilter->SetInput( caster->GetOutput() );
  gradFilter->Update();

  /// Apply a threshold to the Grad(InputImage)
  typedef itk::ThresholdImageFilter< HoughImageType > ThresholdFilterType;

  ThresholdFilterType::Pointer threshFilter = ThresholdFilterType::New();
  threshFilter->SetInput( gradFilter->GetOutput() );
  threshFilter->SetOutsideValue( 0 );
  unsigned char lowerThreshold = 10;
  unsigned char upperThreshold = 200;
  threshFilter->ThresholdOutside( lowerThreshold, upperThreshold );

  threshFilter->Update();

  // Define the HoughTransform filter
  typedef itk::HoughTransform2DLinesImageFilter< HoughSpacePixelType, HoughSpacePixelType >
    HoughTransformFilterType;

  HoughTransformFilterType::Pointer houghFilter = HoughTransformFilterType::New();

  EXERCISE_BASIC_OBJECT_METHODS( houghFilter, HoughTransform2DLinesImageFilter,
    ImageToImageFilter );


  float threshold = 2.3;
  houghFilter->SetThreshold( threshold );
  TEST_SET_GET_VALUE( threshold, houghFilter->GetThreshold() );

  float angleResolution = 200.0;
  houghFilter->SetAngleResolution( angleResolution );
  TEST_SET_GET_VALUE( angleResolution, houghFilter->GetAngleResolution() );

  HoughTransformFilterType::LinesListSizeType numberOfLines =
    static_cast< HoughTransformFilterType::LinesListSizeType >( lines );
  houghFilter->SetNumberOfLines( numberOfLines );
  TEST_SET_GET_VALUE( numberOfLines, houghFilter->GetNumberOfLines() );

  float discRadius = 25.0;
  houghFilter->SetDiscRadius( discRadius );
  TEST_SET_GET_VALUE( discRadius, houghFilter->GetDiscRadius() );

  float variance = 10;
  houghFilter->SetVariance( variance );
  TEST_SET_GET_VALUE( variance, houghFilter->GetVariance() );


  houghFilter->SetInput( threshFilter->GetOutput() );

  houghFilter->Update();

  houghFilter->Simplify();

  HoughImageType::Pointer accumulator = houghFilter->GetOutput();

  HoughImageType::ConstPointer simplifyAccumulator = houghFilter->GetSimplifyAccumulator();


  // Blur the accumulator in order to find the maximum
  typedef itk::DiscreteGaussianImageFilter< HoughImageType,HoughImageType >
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

  itk::ImageRegionIterator< HoughImageType > it_output( m_HoughSpaceImage,
    m_HoughSpaceImage->GetLargestPossibleRegion() );

  itk::ImageRegionIterator< HoughImageType > it_input( postProcessImage,
    postProcessImage->GetLargestPossibleRegion() );


  unsigned int foundLines = 0;

  std::list< HoughPoint > linesList;

  // Search for maxima
  // Each time a maximum is found it is removed by drawing a black disc
  // whose size is defined by the Hough disc radius
  do{
    minMaxCalculator->SetImage( postProcessImage );
    minMaxCalculator->ComputeMaximum();
    HoughImageType::PixelType max = minMaxCalculator->GetMaximum();

    for( it_input.GoToBegin(); !it_input.IsAtEnd(); ++it_input )
    {
      if( itk::Math::ExactlyEquals( it_input.Get(), max ) )
      {
        HoughPoint houghPoint;
        houghPoint.radius = it_input.GetIndex()[0];
        houghPoint.angle  = ( ( it_input.GetIndex()[1] ) *
          2 * itk::Math::pi / houghFilter->GetAngleResolution() ) - itk::Math::pi;

        linesList.push_back( houghPoint );

        // Remove a black disc from the Hough space domain
        for( double angle = 0; angle <= 2 * itk::Math::pi; angle += itk::Math::pi / 1000 )
        {
          for( double length = 0; length < discRadius; length += 1 )
          {
            index[0] = (long int)( it_input.GetIndex()[0] + length * std::cos( angle ) );
            index[1] = (long int)( it_input.GetIndex()[1] + length * std::sin( angle ) );
            if( index[0] <= std::sqrt( (double)400 * 400 + 400 * 400) && index[0] >= 0
              && index[1] <= angleResolution && index[1] >= 0 )
            {
              accumulator->SetPixel( index, 0 );
            }
          }
        }
        minMaxCalculator->SetImage( accumulator );
        minMaxCalculator->ComputeMaximum();
        max = minMaxCalculator->GetMaximum();

        foundLines++;
        if( foundLines == lines )
          {
          break;
          }
      }
    }
  } while( foundLines < lines );

  // Check the line detection
  std::list< HoughPoint >::iterator it_list = linesList.begin();

  double angleTolerance = 0.1;
  double radiusTolerance = 1.0;
  while( it_list != linesList.end() )
    {
    if( !itk::Math::FloatAlmostEqual( it_list->angle, theta, 10, angleTolerance ) )
      {
      std::cout << "Failure for line" << std::endl;
      std::cout << "Expected angle: " << theta << ", found: " << it_list->angle << std::endl;
      success = false;
      }
    if( !itk::Math::FloatAlmostEqual( it_list->radius, radius, 10, radiusTolerance ) )
      {
      std::cout << "Failure for line" << std::endl;
      std::cout << "Expected radius: " << radius << ", found: " << it_list->radius << std::endl;
      success = false;
      }
    else
      {
      std::cout << "Line :" << " angle: " << it_list->angle << ", "
        << "radius: " << it_list->radius << std::endl;
      }
    ++it_list;
    }

  success &= Test_GetLines_should_return_empty_list_when_input_image_is_entirely_black();
  success &= Test_GetLines_should_return_empty_list_when_NumberOfLines_is_set_to_zero();

  if (success)
  {
    std::cout << "Test succeeded!" << std::endl;
    return EXIT_SUCCESS;
  }
  else
  {
    std::cout << "Test FAILED!" << std::endl;
    return EXIT_FAILURE;
  }
}
