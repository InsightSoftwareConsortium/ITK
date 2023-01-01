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

#include "itkBoundedReciprocalImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageRegionConstIterator.h"
#include "itkTestingMacros.h"


int
itkBoundedReciprocalImageFilterTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " inputImage" << std::endl;
    return EXIT_FAILURE;
  }


  auto testStatus = EXIT_SUCCESS;

  constexpr int Dimension = 2;

  using PixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;

  using ReciprocalFilterType = itk::BoundedReciprocalImageFilter<ImageType, ImageType>;

  auto reciprocalFilter = ReciprocalFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(reciprocalFilter, BoundedReciprocalImageFilter, UnaryGeneratorImageFilter);


  const auto inputImage = itk::ReadImage<ImageType>(argv[1]);

  reciprocalFilter->SetInput(inputImage);

  ITK_TRY_EXPECT_NO_EXCEPTION(reciprocalFilter->Update());


  // Check the output image values
  auto outputImage = reciprocalFilter->GetOutput();

  using ImageIterator = itk::ImageRegionConstIterator<ImageType>;

  ImageIterator inIter(inputImage, inputImage->GetBufferedRegion());
  ImageIterator outIter(outputImage, outputImage->GetBufferedRegion());

  double tolerance = 10e-6;
  std::cerr.precision(static_cast<int>(itk::Math::abs(std::log10(tolerance))));

  for (; !inIter.IsAtEnd() || !outIter.IsAtEnd(); ++inIter, ++outIter)
  {
    auto obtainedValue = outIter.Get();
    auto expectedValue = static_cast<PixelType>(1.0 / (1.0 + static_cast<double>(inIter.Get())));
    if (!itk::Math::FloatAlmostEqual(expectedValue, obtainedValue, 10, tolerance))
    {
      std::cerr << "Error at index " << inIter.GetIndex() << std::endl;
      std::cerr << " output " << obtainedValue << std::endl;
      std::cerr << " differs from " << expectedValue;
      std::cerr << " by more than " << tolerance << std::endl;
      testStatus = EXIT_FAILURE;
    }
  }

  std::cout << "Test finished." << std::endl;
  return testStatus;
}
