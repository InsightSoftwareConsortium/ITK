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

#include "itkOtsuThresholdCalculator.h"
#include "itkImageRegionIterator.h"
#include "itkImageToHistogramFilter.h"
#include "itkTestingMacros.h"


int
itkOtsuThresholdCalculatorTest(int, char *[])
{
  constexpr unsigned int Dimension = 3;

  using PixelType = short;
  using SizeType = itk::Size<Dimension>;

  using SizeType = itk::Size<Dimension>;
  using ImageType = itk::Image<PixelType, Dimension>;
  using HistogramGeneratorType = itk::Statistics::ImageToHistogramFilter<ImageType>;
  using HistogramType = HistogramGeneratorType::HistogramType;
  using CalculatorType = itk::OtsuThresholdCalculator<HistogramType>;

  // Allocate a simple test image
  auto                  image = ImageType::New();
  ImageType::RegionType region;

  // Define the image size and physical coordinates
  SizeType size = { { 20, 20, 20 } };

  region.SetSize(size);
  image->SetRegions(region);
  image->Allocate();

  // Set origin and spacing of physical coordinates
  double origin[3] = { 0.0, 0.0, 0.0 };
  double spacing[3] = { 1, 1, 1 };
  image->SetOrigin(origin);
  image->SetSpacing(spacing);

  unsigned long numPixels = region.GetNumberOfPixels();

  using IteratorType = itk::ImageRegionIterator<ImageType>;
  IteratorType iter(image, image->GetBufferedRegion());

  ImageType::PixelType value1 = 10;
  ImageType::PixelType value2 = 50;
  ImageType::PixelType range = 5;
  ImageType::PixelType r2 = range * 2 + 1;

  // Fill one half of with values of value1 +- 2
  unsigned long i;

  for (i = 0; i < numPixels / 2; ++i)
  {
    iter.Set((i % r2) + value1 - range);
    ++iter;
  }

  // Fill the other half with values of value2 +- 2
  for (i = numPixels / 2; i < numPixels; ++i)
  {
    iter.Set((i % r2) + value2 - range);
    ++iter;
  }

  auto histGenerator = HistogramGeneratorType::New();
  histGenerator->SetInput(image);
  HistogramGeneratorType::HistogramSizeType hsize(1);
  hsize[0] = 64;
  histGenerator->SetHistogramSize(hsize);
  histGenerator->SetAutoMinimumMaximum(true);

  // Create and initialize the calculator
  auto calculator = CalculatorType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(calculator, OtsuThresholdCalculator, HistogramThresholdCalculator);

#if defined(ITKV4_COMPATIBILITY)
  bool returnBinMidpoint{ true };
#else
  bool returnBinMidpoint{ false };
#endif
  ITK_TEST_SET_GET_BOOLEAN(calculator, ReturnBinMidpoint, returnBinMidpoint);

  calculator->SetInput(histGenerator->GetOutput());

  calculator->Update();

  // Return minimum of intensity
  double thresholdResult = calculator->GetThreshold();
  std::cout << "The threshold intensity value is : " << thresholdResult << std::endl;

  if (thresholdResult < static_cast<double>(value1) || thresholdResult > static_cast<double>(value2))
  {
    std::cerr << "Test failed!" << std::endl;
    std::cerr << "Error in GetThreshold()" << std::endl;
    std::cerr << "Expected value to be between: " << value1 << " and " << value2 << ", but got: " << thresholdResult
              << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "Test finished" << std::endl;
  return EXIT_SUCCESS;
}
