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

#include "itkAbsImageFilter.h"
#include "itkAbsImageAdaptor.h"
#include "itkImageAdaptor.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMath.h"
#include "itkSubtractImageFilter.h"
#include "itkUnaryGeneratorImageFilter.h"
#include "itkGTest.h"

TEST(AbsImageFilterAndAdaptor, ConvertedLegacyTest)
{
  // Define the dimension of the images
  constexpr unsigned int ImageDimension{ 3 };

  // Declare the types of the images
  using InputImageType = itk::Image<float, ImageDimension>;
  using OutputImageType = itk::Image<float, ImageDimension>;

  // Declare Iterator types appropriate for each image
  using InputIteratorType = itk::ImageRegionIteratorWithIndex<InputImageType>;
  using OutputIteratorType = itk::ImageRegionIteratorWithIndex<OutputImageType>;

  // Declare the type of the size
  using SizeType = itk::Size<ImageDimension>;

  // Declare the type of the Region
  using RegionType = itk::ImageRegion<ImageDimension>;

  // Create two images
  auto inputImage = InputImageType::New();

  // Define their size and region
  constexpr SizeType size{ 2, 2, 2 };
  RegionType         region{ size };

  // Initialize Image A
  inputImage->SetRegions(region);
  inputImage->Allocate();
  // Create one iterator for the Input Image (this is a light object)
  InputIteratorType it(inputImage, inputImage->GetBufferedRegion());

  // Initialize the content of Image A
  const double pi = std::atan(1.0) * 4.0;
  const double value = pi / 6.0;
  it.GoToBegin();
  while (!it.IsAtEnd())
  {
    it.Set(value);
    ++it;
  }

  // Declare the type for the Abs filter
  using FilterType = itk::AbsImageFilter<InputImageType, OutputImageType>;

  // Create an Abs Filter
  auto filter = FilterType::New();

  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(filter, AbsImageFilter, UnaryGeneratorImageFilter);

  // Connect the input images
  filter->SetInput(inputImage);

  // Get the Smart Pointer to the Filter Output
  const OutputImageType::Pointer outputImage = filter->GetOutput();

  // Execute the filter
  filter->Update();

  // Create an iterator for going through the image output
  OutputIteratorType ot(outputImage, outputImage->GetRequestedRegion());

  //  Check the content of the result image
  constexpr OutputImageType::PixelType epsilon{ 1e-6 };
  ot.GoToBegin();
  it.GoToBegin();
  while (!ot.IsAtEnd())
  {
    const InputImageType::PixelType  input = it.Get();
    const OutputImageType::PixelType output = ot.Get();
    const OutputImageType::PixelType absolute = itk::Math::Absolute(input);
    EXPECT_NEAR(absolute, output, epsilon);
    ++ot;
    ++it;
  }

  //
  // Test AbsImageAdaptor
  //

  using AdaptorType = itk::AbsImageAdaptor<InputImageType, OutputImageType::PixelType>;

  auto absAdaptor = AdaptorType::New();

  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(absAdaptor, AbsImageAdaptor, ImageAdaptor);

  absAdaptor->SetImage(inputImage);

  using DiffFilterType = itk::SubtractImageFilter<OutputImageType, AdaptorType, OutputImageType>;

  auto diffFilter = DiffFilterType::New();

  diffFilter->SetInput1(outputImage);
  diffFilter->SetInput2(absAdaptor);

  diffFilter->Update();

  // Get the Smart Pointer to the Diff filter Output
  const OutputImageType::Pointer diffImage = diffFilter->GetOutput();

  //  Check the content of the diff image
  // Create an iterator for going through the image output
  OutputIteratorType dt(diffImage, diffImage->GetRequestedRegion());

  dt.GoToBegin();
  while (!dt.IsAtEnd())
  {
    const OutputImageType::PixelType diff = dt.Get();
    EXPECT_NEAR(diff, OutputImageType::PixelType{ 0 }, epsilon);
    ++dt;
  }
}
