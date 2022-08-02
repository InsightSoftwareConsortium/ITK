/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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


template <typename ImageType, typename CenterCoordinateType>
void
CreateCircle(typename ImageType::Pointer image, const CenterCoordinateType center[Dimension], double radius)
{
  typename ImageType::IndexType index = image->GetLargestPossibleRegion().GetIndex();

  for (double i = 0; i <= radius; i += 0.1)
  {
    for (double angle = 0; angle <= 2 * itk::Math::pi; angle += itk::Math::pi / 1000)
    {
      index[0] = itk::Math::Round<long>(center[0] + i * std::cos(angle));
      index[1] = itk::Math::Round<long>(center[1] + i * std::sin(angle));
      image->SetPixel(index, 255);
    }
  }
}

namespace
{
bool
Test_GetCircles_should_return_empty_list_when_NumberOfCircles_is_set_to_zero()
{
  using PixelType = unsigned char;

  using ImageType = itk::Image<PixelType>;

  // Create an image that has at least one circle.
  const auto                image = ImageType::New();
  const ImageType::SizeType size = { { 64, 64 } };
  image->SetRegions(size);
  image->Allocate(true);
  const unsigned int center[] = { 16, 16 };
  constexpr double   radius = 7.0;
  CreateCircle<ImageType>(image, center, radius);

  using FilterType = itk::HoughTransform2DCirclesImageFilter<PixelType, PixelType, PixelType>;

  const auto filter = FilterType::New();

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


bool
Test_GetCircles_should_return_empty_list_when_input_image_is_uniform()
{
  using PixelType = unsigned char;

  using ImageType = itk::Image<PixelType>;

  using FilterType = itk::HoughTransform2DCirclesImageFilter<PixelType, PixelType, PixelType>;

  const auto filter = FilterType::New();

  // Create an input image for the filter.
  const auto                image = ImageType::New();
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
bool
Test_RadiusImage_and_OutputImage_may_have_different_types()
{
  using InputPixelType = unsigned char;

  using InputImageType = itk::Image<InputPixelType>;

  // Create an image that has at least one circle.
  const auto                     inputImage = InputImageType::New();
  const InputImageType::SizeType size = { { 64, 64 } };
  inputImage->SetRegions(size);
  inputImage->Allocate();
  inputImage->FillBuffer(1);
  const unsigned int center[] = { 32, 32 };
  constexpr double   radius = 8.5;
  CreateCircle<InputImageType>(inputImage, center, radius);

  using OutputPixelType = unsigned long;

  // FilterType2 has 'double' as radius pixel type, allowing a slightly more accurate radius estimation.
  using FilterType1 = itk::HoughTransform2DCirclesImageFilter<InputPixelType, OutputPixelType, unsigned long>;
  using FilterType2 = itk::HoughTransform2DCirclesImageFilter<InputPixelType, OutputPixelType, double>;

  const auto filter1 = FilterType1::New();
  const auto filter2 = FilterType2::New();

  filter1->SetInput(inputImage);
  filter2->SetInput(inputImage);
  filter1->Update();
  filter2->Update();

  const FilterType1::RadiusImageType * const radiusImage1 = filter1->GetRadiusImage();
  const FilterType2::RadiusImageType * const radiusImage2 = filter2->GetRadiusImage();

  if ((radiusImage1 == nullptr) || (radiusImage2 == nullptr))
  {
    std::cout << "GetRadiusImage() should not return NULL!" << std::endl;
    return false;
  }

  // Note that GetBufferPointer() returns a different type for filter2 than for filter1.
  const OutputPixelType * const radiusBufferPointer1 = radiusImage1->GetBufferPointer();
  const double * const          radiusBufferPointer2 = radiusImage2->GetBufferPointer();

  if ((radiusBufferPointer1 == nullptr) || (radiusBufferPointer2 == nullptr))
  {
    std::cout << "A GetBufferPointer() call appears to fail!" << std::endl;
    return false;
  }

  using CirclesListType = FilterType1::CirclesListType;

  const CirclesListType & circles1 = filter1->GetCircles();
  const CirclesListType & circles2 = filter2->GetCircles();

  if (circles1.empty() || circles2.empty())
  {
    std::cout << "This test was expecting to find a circle!" << std::endl;
    return false;
  }

  if (circles1.size() != circles2.size())
  {
    // The choice of the radius image type should not affect the number of
    // circles found.
    std::cout << "The size of circles1 and circles2 should be equal, even"
              << " while the radius image types differ!" << std::endl;
    return false;
  }

  using CircleType = FilterType1::CircleType;

  const CircleType * const circle1 = circles1.front().GetPointer();
  const CircleType * const circle2 = circles2.front().GetPointer();

  if ((circle1 == nullptr) || (circle2 == nullptr))
  {
    std::cout << "A Circle pointer appears to be incorrect!" << std::endl;
    return false;
  }

  bool success = true;

  using PointType = CircleType::PointType;

  const PointType & centerPoint1 = circle1->GetCenterInObjectSpace();
  const PointType & centerPoint2 = circle2->GetCenterInObjectSpace();

  if (centerPoint1 != centerPoint2)
  {
    // The choice of the radius image type should not affect the center
    // estimation.
    std::cout << "center1 and center2 should be equal, even while the "
              << "radius image types differ!" << std::endl;
    success = false;
  }

  const double radius1 = circle1->GetRadiusInObjectSpace()[0];
  const double radius2 = circle2->GetRadiusInObjectSpace()[0];

  if (radius2 < radius1)
  {
    // The radius estimation of filter1 was truncated, whereas the radius
    // estimation of filter2 was not,
    // so radius2 is expected to be greater than or equal to radius1.
    std::cout << "radius2 (radius image type double) should be >= radius1"
              << " (radius image type unsigned long)!" << std::endl;
    success = false;
  }

  constexpr double radiusTolerance = 1.0;

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


// Tests that the center of a circle that was created on the input image
// is inside the spatial object produced by GetCircles().
bool
Test_Center_IsInside_SpatialObject_from_GetCircles()
{
  using PixelType = unsigned int;
  using ImageType = itk::Image<PixelType>;
  const auto                image = ImageType::New();
  const ImageType::SizeType imageSize = { { 16, 32 } };
  image->SetRegions(imageSize);
  image->Allocate(true);
  const double center[] = { 6.0, 9.0 };
  const double radius = 1.0;
  CreateCircle<ImageType>(image, center, radius);

  using FilterType = itk::HoughTransform2DCirclesImageFilter<PixelType, unsigned int, double>;
  const auto filter = FilterType::New();
  filter->SetInput(image);
  filter->Update();

  const FilterType::CirclesListType & circles = filter->GetCircles();

  if (circles.size() != 1)
  {
    std::cout << "ERROR: GetCircles() should have found exactly one circle!" << std::endl;
    return false;
  }

  const FilterType::CirclePointer & circle = circles.front();

  if (circle == nullptr)
  {
    std::cout << "ERROR: The circle found by GetCircles() should not be null!" << std::endl;
    return false;
  }

  const bool isInside = circle->IsInsideInWorldSpace(center);

  if (!isInside)
  {
    std::cout << "ERROR: The center of the actual circle should be inside the"
              << " spacial object of the detected circle!" << std::endl;
    std::cout << circle << std::endl;
  }
  return isInside;
}

} // namespace

int
itkHoughTransform2DCirclesImageTest(int, char *[])
{
  bool success = true;

  // Declare the pixel types of the images
  using PixelType = unsigned char;
  using HoughSpacePixelType = double;

  // Declare the types of the images
  using HoughImageType = itk::Image<HoughSpacePixelType, Dimension>;
  using ImageType = itk::Image<PixelType, Dimension>;

  // Create a black image
  auto image = ImageType::New();

  ImageType::RegionType region;

  ImageType::SizeType size;
  size.Fill(100);

  ImageType::IndexType index;
  index.Fill(0);

  region.SetSize(size);
  region.SetIndex(index);

  image->SetRegions(region);
  image->Allocate(true); // initialize buffer to zero

  // Create 3 circles
  constexpr unsigned int circles = 3;

  unsigned int center[circles][Dimension];
  double       radius[circles];

  center[0][0] = 50;
  center[0][1] = 50;
  radius[0] = 15;

  center[1][0] = 25;
  center[1][1] = 25;
  radius[1] = 7;

  center[2][0] = 71;
  center[2][1] = 72;
  radius[2] = 5;

  for (unsigned int i = 0; i < circles; ++i)
  {
    CreateCircle<ImageType>(image, center[i], radius[i]);
  }

  // Allocate Hough Space image (accumulator)
  auto m_HoughSpaceImage = ImageType::New();
  m_HoughSpaceImage->SetRegions(region);
  m_HoughSpaceImage->Allocate(true); // initialize buffer to zero

  // Apply gradient filter to the input image
  using CastingFilterType = itk::CastImageFilter<ImageType, HoughImageType>;

  auto caster = CastingFilterType::New();
  caster->SetInput(image);

  // Define the HoughTransform filter
  using HoughTransformFilterType =
    itk::HoughTransform2DCirclesImageFilter<HoughSpacePixelType, HoughSpacePixelType, HoughSpacePixelType>;

  auto houghFilter = HoughTransformFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(houghFilter, HoughTransform2DCirclesImageFilter, ImageToImageFilter);


  double threshold = 2.3;
  houghFilter->SetThreshold(threshold);
  ITK_TEST_SET_GET_VALUE(threshold, houghFilter->GetThreshold());

  double minMaxRadius = 16.2;
  houghFilter->SetRadius(minMaxRadius);
  ITK_TEST_SET_GET_VALUE(minMaxRadius, houghFilter->GetMinimumRadius());
  ITK_TEST_SET_GET_VALUE(minMaxRadius, houghFilter->GetMaximumRadius());

  double minimumRadius = 2.1;
  houghFilter->SetMinimumRadius(minimumRadius);
  ITK_TEST_SET_GET_VALUE(minimumRadius, houghFilter->GetMinimumRadius());

  double maximumRadius = 20.4;
  houghFilter->SetMaximumRadius(maximumRadius);
  ITK_TEST_SET_GET_VALUE(maximumRadius, houghFilter->GetMaximumRadius());

  const double gradientNormThreshold = 1.1;
  houghFilter->SetGradientNormThreshold(gradientNormThreshold);
  ITK_TEST_SET_GET_VALUE(gradientNormThreshold, houghFilter->GetGradientNormThreshold());

  double sigmaGradient = 1.2;
  houghFilter->SetSigmaGradient(sigmaGradient);
  ITK_TEST_SET_GET_VALUE(sigmaGradient, houghFilter->GetSigmaGradient());

  float discRadiusRatio = 1.1;
  houghFilter->SetDiscRadiusRatio(discRadiusRatio);
  ITK_TEST_SET_GET_VALUE(discRadiusRatio, houghFilter->GetDiscRadiusRatio());

  float variance = 10;
  houghFilter->SetVariance(variance);
  ITK_TEST_SET_GET_VALUE(variance, houghFilter->GetVariance());

  float sweepAngle = 0.2;
  houghFilter->SetSweepAngle(sweepAngle);
  ITK_TEST_SET_GET_VALUE(sweepAngle, houghFilter->GetSweepAngle());

  auto numberOfCircles = static_cast<HoughTransformFilterType::CirclesListSizeType>(circles);
  houghFilter->SetNumberOfCircles(numberOfCircles);
  ITK_TEST_SET_GET_VALUE(numberOfCircles, houghFilter->GetNumberOfCircles());

  bool useImageSpacing = false;
  houghFilter->SetUseImageSpacing(useImageSpacing);
  ITK_TEST_SET_GET_VALUE(useImageSpacing, houghFilter->GetUseImageSpacing());

  houghFilter->SetInput(caster->GetOutput());

  ITK_TRY_EXPECT_EXCEPTION(houghFilter->GetCircles());

  houghFilter->Update();

  // Check the circle radius
  HoughTransformFilterType::CirclesListType circleList = houghFilter->GetCircles();

  circleList = houghFilter->GetCircles();

  double radiusTolerance = 2.0;

  HoughTransformFilterType::CirclesListType::const_iterator it = circleList.begin();

  unsigned int i = 0;
  while (it != circleList.end())
  {
    if (!itk::Math::FloatAlmostEqual(
          static_cast<double>(it->GetPointer()->GetRadiusInObjectSpace()[0]), radius[i], 10, radiusTolerance) &&
        !itk::Math::FloatAlmostEqual(static_cast<double>(it->GetPointer()->GetRadiusInObjectSpace()[0]),
                                     radius[i] * discRadiusRatio,
                                     10,
                                     radiusTolerance))
    {
      std::cout << "Failure for circle #" << i << std::endl;
      std::cout << "Expected radius: " << radius[i] << ", found " << it->GetPointer()->GetRadiusInObjectSpace()
                << std::endl;
      success = false;
    }
    else
    {
      std::cout << "Circle #" << i << " radius: " << it->GetPointer()->GetRadiusInObjectSpace() << std::endl;
    }
    ++it;
    ++i;
  }

  // Check the circle center
  HoughImageType::Pointer accumulator = houghFilter->GetOutput();

  HoughImageType::ConstPointer radiusImage = houghFilter->GetRadiusImage();

  // Blur the accumulator in order to find the maximum
  using GaussianFilterType = itk::DiscreteGaussianImageFilter<HoughImageType, HoughImageType>;

  auto gaussianFilter = GaussianFilterType::New();
  gaussianFilter->SetInput(accumulator);
  double gaussianFilterVariance[Dimension];
  gaussianFilterVariance[0] = variance;
  gaussianFilterVariance[1] = variance;
  gaussianFilter->SetVariance(gaussianFilterVariance);
  gaussianFilter->SetMaximumError(.01f);

  gaussianFilter->Update();

  HoughImageType::Pointer postProcessImage = gaussianFilter->GetOutput();

  using MinMaxCalculatorType = itk::MinimumMaximumImageCalculator<HoughImageType>;
  auto minMaxCalculator = MinMaxCalculatorType::New();

  itk::ImageRegionIterator<ImageType> it_output(m_HoughSpaceImage, m_HoughSpaceImage->GetLargestPossibleRegion());

  itk::ImageRegionIterator<HoughImageType> it_input(postProcessImage, postProcessImage->GetLargestPossibleRegion());


  // Search for maxima
  unsigned int centerResult[circles][Dimension];
  double       radiusResult[circles];
  unsigned int foundCircles = 0;
  do
  {
    minMaxCalculator->SetImage(postProcessImage);
    minMaxCalculator->ComputeMaximum();

    HoughImageType::PixelType max = minMaxCalculator->GetMaximum();

    it_output.GoToBegin();
    for (it_input.GoToBegin(); !it_input.IsAtEnd(); ++it_input)
    {
      if (itk::Math::ExactlyEquals(it_input.Get(), max))
      {
        it_output.Set(255);
        double radius2 = radiusImage->GetPixel(it_output.GetIndex());
        centerResult[foundCircles][0] = it_output.GetIndex()[0];
        centerResult[foundCircles][1] = it_output.GetIndex()[1];
        radiusResult[foundCircles] = radius2;

        // Draw the circle
        for (double angle = 0; angle <= 2 * itk::Math::pi; angle += itk::Math::pi / 1000)
        {
          index[0] = itk::Math::Round<long>(it_output.GetIndex()[0] + radius2 * std::cos(angle));
          index[1] = itk::Math::Round<long>(it_output.GetIndex()[1] + radius2 * std::sin(angle));
          m_HoughSpaceImage->SetPixel(index, 255);

          // Remove the maximum from the accumulator
          for (double length = 0; length < discRadiusRatio * radius2; length += 1)
          {
            index[0] = itk::Math::Round<long>(it_output.GetIndex()[0] + length * std::cos(angle));
            index[1] = itk::Math::Round<long>(it_output.GetIndex()[1] + length * std::sin(angle));
            postProcessImage->SetPixel(index, 0);
          }
        }

        minMaxCalculator->SetImage(postProcessImage);
        minMaxCalculator->ComputeMaximum();
        max = minMaxCalculator->GetMaximum();

        foundCircles++;
        if (foundCircles == numberOfCircles)
        {
          break;
        }
      }
      ++it_output;
    }
  } while (foundCircles < numberOfCircles);

  // Check the circle detection
  double centerTolerance = 2.0;
  for (i = 0; i < numberOfCircles; ++i)
  {
    if (!itk::Math::FloatAlmostEqual(
          static_cast<double>(centerResult[i][0]), static_cast<double>(center[i][0]), 10, centerTolerance) ||
        !itk::Math::FloatAlmostEqual(
          static_cast<double>(centerResult[i][1]), static_cast<double>(center[i][1]), 10, centerTolerance) ||
        (!itk::Math::FloatAlmostEqual(radiusResult[i], radius[i], 10, radiusTolerance) &&
         !itk::Math::FloatAlmostEqual(radiusResult[i], radius[i] * discRadiusRatio, 10, radiusTolerance)))
    {
      std::cout << "Failure for circle #" << i << std::endl;
      std::cout << "Expected center: [" << center[i][0] << ", " << center[i][1] << "], found [" << centerResult[i][0]
                << ", " << centerResult[i][1] << "]" << std::endl;
      std::cout << "Expected radius: " << radius[i] << ", found " << radiusResult[i] << std::endl;
      success = false;
    }
    else
    {
      std::cout << "Circle #" << i << " [" << centerResult[i][0] << ", " << centerResult[i][1]
                << "] -> radius: " << radiusResult[i] << std::endl;
    }
  }

  success &= Test_GetCircles_should_return_empty_list_when_NumberOfCircles_is_set_to_zero();
  success &= Test_GetCircles_should_return_empty_list_when_input_image_is_uniform();
  success &= Test_RadiusImage_and_OutputImage_may_have_different_types();
  success &= Test_Center_IsInside_SpatialObject_from_GetCircles();

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
