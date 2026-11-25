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

// First include the header file to be tested:
#include "itkSobelEdgeDetectionImageFilter.h"

#include "itkDeref.h"
#include "itkImage.h"
#include "itkImageBufferRange.h"

#include <gtest/gtest.h>

#include <cmath> // For sqrt.
#include <vector>


// Checks that the filter uses legacy Sobel operator coefficients by default, unless ITK_FUTURE_LEGACY_REMOVE is
// enabled.
TEST(SobelEdgeDetectionImageFilter, UseLegacyOperatorCoefficientsByDefaultUnlessFutureLegacyRemove)
{
  const auto filter = itk::SobelEdgeDetectionImageFilter<itk::Image<int>, itk::Image<double>>::New();
#ifdef ITK_FUTURE_LEGACY_REMOVE
  EXPECT_FALSE(filter->GetUseLegacyOperatorCoefficients());
#else
  EXPECT_TRUE(filter->GetUseLegacyOperatorCoefficients());
#endif
}


// Checks the output for a minimal 3D example, both when using legacy and when using non-legacy Sobel operator
// coefficients.
TEST(SobelEdgeDetectionImageFilter, UseLegacyOperatorCoefficientsFor3D)
{
  constexpr unsigned int dimension{ 3 };

  using InputImageType = itk::Image<int, dimension>;
  using OutputImageType = itk::Image<double, dimension>;

  const auto     inputImage = InputImageType::New();
  constexpr auto imageSize = itk::Size<dimension>::Filled(3);
  inputImage->SetRegions(imageSize);
  inputImage->AllocateInitialized();
  inputImage->SetPixel(itk::Index<dimension>::Filled(1), 1);

  constexpr auto sqrtOfValues = [](auto values) {
    for (auto & value : values)
    {
      value = std::sqrt(value);
    }
    return values;
  };

  for (const bool useLegacyCoefficients : { true, false })
  {
    const auto filter = itk::SobelEdgeDetectionImageFilter<InputImageType, OutputImageType>::New();
    filter->SetInput(inputImage);
    filter->SetUseLegacyOperatorCoefficients(useLegacyCoefficients);
    filter->Update();
    const OutputImageType & outputImage = itk::Deref(filter->GetOutput());

    const itk::ImageBufferRange outputImageBufferRange(outputImage);

    // Note that the expected pixel values are square roots, because the filter also computes the square root, after
    // convolving the coefficients with the pixels.
    const auto expectedPixelValues =
      sqrtOfValues(useLegacyCoefficients ? std::vector<double>{ 3,  18, 3,  18, 36, 18, 3, 18, 3,  18, 36, 18, 36, 0,
                                                                36, 18, 36, 18, 3,  18, 3, 18, 36, 18, 3,  18, 3 }
                                         : std::vector<double>{ 3,  8, 3,  8, 16, 8, 3, 8, 3,  8, 16, 8, 16, 0,
                                                                16, 8, 16, 8, 3,  8, 3, 8, 16, 8, 3,  8, 3 });

    const std::vector<double> actualPixelValues(outputImageBufferRange.cbegin(), outputImageBufferRange.cend());

    EXPECT_EQ(actualPixelValues.size(), expectedPixelValues.size());
    EXPECT_EQ(actualPixelValues, expectedPixelValues);
  }
}
