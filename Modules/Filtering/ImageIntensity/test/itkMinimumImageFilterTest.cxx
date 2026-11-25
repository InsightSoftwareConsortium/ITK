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

#include "itkMinimumImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkTestingMacros.h"


int
itkMinimumImageFilterTest(int, char *[])
{

  // Define the dimension of the images
  constexpr unsigned int Dimension{ 3 };

  using PixelType = unsigned char;

  // Declare the types of the images
  using ImageType = itk::Image<PixelType, Dimension>;

  // Declare the type of the size
  using SizeType = itk::Size<Dimension>;

  // Declare the type of the Region
  using RegionType = itk::ImageRegion<Dimension>;

  // Declare the type for the MULTIPLY filter
  using MinimumFilterType = itk::MinimumImageFilter<ImageType, ImageType, ImageType>;

  // Create two images
  auto inputImageA = ImageType::New();
  auto inputImageB = ImageType::New();

  // Define their size and region
  constexpr SizeType size{ 2, 2, 2 };
  RegionType         region{ size };

  // Initialize Image A
  inputImageA->SetRegions(region);
  inputImageA->Allocate();

  // Initialize Image B
  inputImageB->SetRegions(region);
  inputImageB->Allocate();

  // Define the pixel values for each image
  constexpr PixelType largePixelValue{ 3 };
  constexpr PixelType smallPixelValue{ 2 };

  // Initialize the content of Image A
  inputImageA->FillBuffer(smallPixelValue);

  // Initialize the content of Image B
  inputImageB->FillBuffer(largePixelValue);

  // Create the filter
  auto minimumImageFilter = MinimumFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(minimumImageFilter, MinimumImageFilter, BinaryGeneratorImageFilter);

  // Connect the input images
  minimumImageFilter->SetInput1(inputImageA);
  minimumImageFilter->SetInput2(inputImageB);

  // Get the Smart Pointer to the filter output
  const ImageType::Pointer outputImage = minimumImageFilter->GetOutput();


  // Execute the filter
  minimumImageFilter->Update();

  // Test some pixels in the result image
  // Note that we are not comparing the entirety of the filter output in order
  // to keep compile time as small as possible

  constexpr ImageType::IndexType pixelIndex{ 0, 1, 1 };

  ITK_TEST_EXPECT_EQUAL(outputImage->GetPixel({}), smallPixelValue);
  ITK_TEST_EXPECT_EQUAL(outputImage->GetPixel(pixelIndex), smallPixelValue);

  // All objects should be automatically destroyed at this point
  return EXIT_SUCCESS;
}
