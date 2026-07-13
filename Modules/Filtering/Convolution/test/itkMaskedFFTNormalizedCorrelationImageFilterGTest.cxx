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

#include "itkGTest.h"

#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkMaskedFFTNormalizedCorrelationImageFilter.h"
#include "itkMinimumMaximumImageCalculator.h"
#include "itkTestDriverIncludeRequiredFactories.h"

namespace
{
using InputImageType = itk::Image<int, 2>;
using RealImageType = itk::Image<double, 2>;
using FilterType = itk::MaskedFFTNormalizedCorrelationImageFilter<InputImageType, RealImageType>;

class MaskedFFTNormalizedCorrelationImageFilterTestSuite : public ::testing::Test
{
protected:
  static void
  SetUpTestSuite()
  {
    RegisterRequiredFactories();
  }
};

InputImageType::Pointer
MakeCheckerboardImage(InputImageType::PixelType highValue, InputImageType::PixelType lowValue, unsigned int size)
{
  auto                             image = InputImageType::New();
  const InputImageType::RegionType region(InputImageType::SizeType::Filled(size));
  image->SetRegions(region);
  image->Allocate();
  for (itk::ImageRegionIteratorWithIndex<InputImageType> it(image, region); !it.IsAtEnd(); ++it)
  {
    const auto idx = it.GetIndex();
    it.Set(((idx[0] + idx[1]) % 2 == 0) ? highValue : lowValue);
  }
  return image;
}
} // namespace

// Pixel value 50000 exceeds sqrt(INT32_MAX), so the denominator sums require real-typed squaring.
TEST_F(MaskedFFTNormalizedCorrelationImageFilterTestSuite, SelfCorrelationAlternatesSignForLargeIntegerPixels)
{
  auto image = MakeCheckerboardImage(50000, 0, 7);

  auto filter = FilterType::New();
  filter->SetFixedImage(image);
  filter->SetMovingImage(image);
  filter->Update();

  using CalculatorType = itk::MinimumMaximumImageCalculator<RealImageType>;
  auto calculator = CalculatorType::New();
  calculator->SetImage(filter->GetOutput());
  calculator->ComputeMaximum();
  calculator->ComputeMinimum();

  EXPECT_NEAR(calculator->GetMaximum(), 1.0, 1e-9);
  EXPECT_NEAR(calculator->GetMinimum(), -1.0, 1e-9);
}
