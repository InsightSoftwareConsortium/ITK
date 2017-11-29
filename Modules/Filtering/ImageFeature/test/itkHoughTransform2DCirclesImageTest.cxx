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
void CreateCircle( typename ImageType::Pointer image, const unsigned int center[Dimension], double radius )
{
  typename ImageType::IndexType index = image->GetLargestPossibleRegion().GetIndex();

  for( double i = 0; i <= radius; i += 0.1 )
  {
    for( double angle = 0; angle <= 2 * itk::Math::pi; angle += itk::Math::pi / 1000 )
    {
      index[0] = itk::Math::Round<long int>( center[0] + i * std::cos( angle ) );
      index[1] = itk::Math::Round<long int>( center[1] + i * std::sin( angle ) );
      image->SetPixel( index, 255 );
    }
  }
}

namespace
{
  bool Test_GetCircles_should_return_empty_list_when_NumberOfCircles_is_set_to_zero()
  {
    typedef unsigned char PixelType;

    typedef itk::Image<PixelType> ImageType;

    // Create an image that has at least one circle.
    const ImageType::Pointer image = ImageType::New();
    const ImageType::SizeType size = { { 64, 64 } };
    image->SetRegions(size);
    image->Allocate(true);
    const unsigned int center[] = { 16, 16 };
    const double radius = 7.0;
    CreateCircle<ImageType>(image, center, radius);

    typedef itk::HoughTransform2DCirclesImageFilter< PixelType, PixelType > FilterType;

    const FilterType::Pointer filter = FilterType::New();

    filter->SetInput(image);
    filter->SetNumberOfCircles(0);
    filter->Update();

    if (!filter->GetCircles().empty())
    {
      std::cout << "GetCircles() should return an empty list when NumberOfCircles is set to zero" << std::endl;
      return false;
    }
    return true;
  }


  bool Test_GetCircles_should_return_empty_list_when_input_image_is_uniform()
  {
    typedef unsigned char PixelType;

    typedef itk::Image<PixelType> ImageType;

    typedef itk::HoughTransform2DCirclesImageFilter< PixelType, PixelType > FilterType;

    const FilterType::Pointer filter = FilterType::New();

    // Create an input image for the filter.
    const ImageType::Pointer image = ImageType::New();
    const ImageType::SizeType size = { { 32, 32 } };
    image->SetRegions(size);
    image->Allocate();

    filter->SetInput(image);

    // Test for uniform input images of pixel value 0, 1, and 2 (which should be sufficient).
    for (PixelType pixelValue = 0; pixelValue <= 2; ++pixelValue)
    {
      image->FillBuffer(pixelValue);
      image->Modified();
      filter->Update();

      if (!filter->GetCircles().empty())
      {
        std::cout << "GetCircles() should return an empty list when the input image is uniform" << std::endl;
        return false;
      }
    }

    return true;
  }


  // Tests that RadiusImage and OutputImage may have different types.
  // The estimated centers of the found circles should not be affected
  // by changing the RadiusImage type, but the estimated radii may be
  // slightly affected.
  bool Test_RadiusImage_and_OutputImage_may_have_different_types()
  {
    typedef unsigned char InputPixelType;

    typedef itk::Image<InputPixelType> InputImageType;

    // Create an image that has at least one circle.
    const InputImageType::Pointer inputImage = InputImageType::New();
    const InputImageType::SizeType size = { { 64, 64 } };
    inputImage->SetRegions(size);
    inputImage->Allocate();
    inputImage->FillBuffer(1);
    const unsigned int center[] = { 32, 32 };
    const double radius = 8.5;
    CreateCircle<InputImageType>(inputImage, center, radius);

    typedef unsigned long OutputPixelType;

    // By default, the radius image has the same type as the output image (the accumulator image).
    // FilterType2 has 'double' as radius pixel type, allowing a slightly more accurate radius estimation.
    typedef itk::HoughTransform2DCirclesImageFilter< InputPixelType, OutputPixelType >         FilterType1;
    typedef itk::HoughTransform2DCirclesImageFilter< InputPixelType, OutputPixelType, double > FilterType2;

    const FilterType1::Pointer filter1 = FilterType1::New();
    const FilterType2::Pointer filter2 = FilterType2::New();

    filter1->SetInput(inputImage);
    filter2->SetInput(inputImage);
    filter1->Update();
    filter2->Update();

    const FilterType1::RadiusImageType* const radiusImage1 = filter1->GetRadiusImage();
    const FilterType2::RadiusImageType* const radiusImage2 = filter2->GetRadiusImage();

    if ( (radiusImage1 == ITK_NULLPTR) || (radiusImage2 == ITK_NULLPTR) )
    {
      std::cout << "GetRadiusImage() should not return NULL!" << std::endl;
      return false;
    }

    // Note that GetBufferPointer() returns a different type for filter2 than for filter1.
    const OutputPixelType* const radiusBufferPointer1 = radiusImage1->GetBufferPointer();
    const double* const radiusBufferPointer2 = radiusImage2->GetBufferPointer();

    if ( (radiusBufferPointer1 == ITK_NULLPTR) || (radiusBufferPointer2 == ITK_NULLPTR) )
    {
      std::cout << "A GetBufferPointer() call appears to fail!" << std::endl;
      return false;
    }

    typedef FilterType1::CirclesListType CirclesListType;

    const CirclesListType& circles1 = filter1->GetCircles();
    const CirclesListType& circles2 = filter2->GetCircles();

    if ( circles1.empty() || circles2.empty() )
    {
      std::cout << "This test was expecting to find a circle!" << std::endl;
      return false;
    }

    if ( circles1.size() != circles2.size() )
    {
      // The choice of the radius image type should not affect the number of circles found.
      std::cout << "The size of circles1 and circles2 should be equal, even while the radius image types differ!"
        << std::endl;
      return false;
    }

    typedef FilterType1::CircleType CircleType;

    const CircleType* const circle1 = circles1.front().GetPointer();
    const CircleType* const circle2 = circles2.front().GetPointer();

    if ( (circle1 == ITK_NULLPTR) || (circle2 == ITK_NULLPTR) )
    {
      std::cout << "A Circle pointer appears to be incorrect!" << std::endl;
      return false;
    }

    const CircleType::TransformType* const transform1 = circle1->GetObjectToParentTransform();
    const CircleType::TransformType* const transform2 = circle2->GetObjectToParentTransform();

    if ( (transform1 == ITK_NULLPTR) || (transform2 == ITK_NULLPTR) )
    {
      std::cout << "A GetObjectToParentTransform() call appears to be incorrect!" << std::endl;
      return false;
    }

    bool success = true;

    const itk::Vector<double, 2>& center1 = transform1->GetOffset();
    const itk::Vector<double, 2>& center2 = transform2->GetOffset();

    if (center1 != center2)
    {
      // The choice of the radius image type should not affect the center estimation.
      std::cout << "center1 and center2 should be equal, even while the radius image types differ!"
        << std::endl;
      success = false;
    }

    const double radius1 = circle1->GetRadius()[0];
    const double radius2 = circle2->GetRadius()[0];

    if ( radius2 < radius1 )
    {
      // The radius estimation of filter1 was truncated, whereas the radius estimation of filter2 was not,
      // so radius2 is expected to be greater than or equal to radius1.
      std::cout << "radius2 (radius image type double) should be >= radius1 (radius image type unsigned long)!"
        << std::endl;
      success = false;
    }

    const double radiusTolerance = 1.0;

    if (!itk::Math::FloatAlmostEqual(radius1, radius, 0, radiusTolerance))
    {
      std::cout << "Expected radius: " << radius << ", found radius1 = " << radius1 << std::endl;
      success = false;
    }

    if (!itk::Math::FloatAlmostEqual(radius2, radius, 0, radiusTolerance))
    {
      std::cout << "Expected radius: " << radius << ", found radius2 = " << radius2 << std::endl;
      success = false;
    }

    return success;
  }

}

int itkHoughTransform2DCirclesImageTest( int, char* [] )
{
  bool success = true;

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

  houghFilter->SetInput( caster->GetOutput() );

  TRY_EXPECT_EXCEPTION( houghFilter->GetCircles() );

  houghFilter->Update();

  // Check the circle radius
  HoughTransformFilterType::CirclesListType circleList =
    houghFilter->GetCircles();

  circleList = houghFilter->GetCircles();

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
      success = false;
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
          index[0] = itk::Math::Round<long int>( it_output.GetIndex()[0] + radius2 * std::cos( angle ) );
          index[1] = itk::Math::Round<long int>( it_output.GetIndex()[1] + radius2 * std::sin( angle ) );
          m_HoughSpaceImage->SetPixel( index, 255 );

          // Remove the maximum from the accumulator
          for( double length = 0; length < discRadiusRatio * radius2; length += 1 )
          {
            index[0] = itk::Math::Round<long int>( it_output.GetIndex()[0] + length * std::cos( angle ) );
            index[1] = itk::Math::Round<long int>( it_output.GetIndex()[1] + length * std::sin( angle ) );
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
      success = false;
      }
    else
      {
      std::cout << "Circle #" << i << " [" << centerResult[i][0] << ", "
                << centerResult[i][1] << "] -> radius: " << radiusResult[i] << std::endl;
      }
    }

  success &= Test_GetCircles_should_return_empty_list_when_NumberOfCircles_is_set_to_zero();
  success &= Test_GetCircles_should_return_empty_list_when_input_image_is_uniform();
  success &= Test_RadiusImage_and_OutputImage_may_have_different_types();

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
