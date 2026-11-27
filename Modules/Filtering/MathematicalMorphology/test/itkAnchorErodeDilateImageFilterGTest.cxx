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

#include "itkAnchorDilateImageFilter.h"
#include "itkAnchorErodeImageFilter.h"
#include "itkClosingByReconstructionImageFilter.h"
#include "itkFlatStructuringElement.h"
#include "itkGTest.h"

#include <algorithm>
#include <cmath>
#include <functional>
#include <vector>

namespace
{
using ImageType = itk::Image<float, 2>;
using KernelType = itk::FlatStructuringElement<2>;

// A plateau whose neighboring values differ by one ULP.
std::vector<float>
MakeUlpStaircase(float background, float direction)
{
  std::vector<float> values(16, background);
  float              value = 50.0f;
  for (size_t i = 4; i < 12; ++i)
  {
    values[i] = value;
    value = std::nextafter(value, direction);
  }
  return values;
}

// Identical rows, so the expected result in every row is the 1D extreme along the row.
ImageType::Pointer
MakeImage(const std::vector<float> & values)
{
  constexpr size_t numberOfRows = 3;
  auto             image = ImageType::New();
  image->SetRegions(ImageType::RegionType(ImageType::SizeType{ { values.size(), numberOfRows } }));
  image->Allocate();
  for (size_t row = 0; row < numberOfRows; ++row)
  {
    std::copy(values.cbegin(), values.cend(), image->GetBufferPointer() + row * values.size());
  }
  return image;
}

template <typename TFilter, typename TCompare>
void
ExpectExactExtremes(const std::vector<float> & values, TCompare compare)
{
  auto filter = TFilter::New();
  filter->SetInput(MakeImage(values));
  filter->SetKernel(KernelType::Box(KernelType::RadiusType::Filled(1)));
  filter->Update();

  const float * const output = filter->GetOutput()->GetBufferPointer();
  for (size_t i = 0; i < filter->GetOutput()->GetBufferedRegion().GetNumberOfPixels(); ++i)
  {
    const size_t x = i % values.size();
    const auto   first = values.cbegin() + (x == 0 ? 0 : x - 1);
    const auto   last = values.cbegin() + std::min(x + 2, values.size());
    EXPECT_EQ(output[i], *std::min_element(first, last, compare)) << "pixel " << i;
  }
}
} // namespace

TEST(AnchorErodeDilateImageFilter, DilateIsExactForNearlyEqualFloats)
{
  using FilterType = itk::AnchorDilateImageFilter<ImageType, KernelType>;
  ExpectExactExtremes<FilterType>(MakeUlpStaircase(0.0f, 0.0f), std::greater<float>());
  ExpectExactExtremes<FilterType>(MakeUlpStaircase(0.0f, 100.0f), std::greater<float>());
}

TEST(AnchorErodeDilateImageFilter, ErodeIsExactForNearlyEqualFloats)
{
  using FilterType = itk::AnchorErodeImageFilter<ImageType, KernelType>;
  ExpectExactExtremes<FilterType>(MakeUlpStaircase(100.0f, 0.0f), std::less<float>());
  ExpectExactExtremes<FilterType>(MakeUlpStaircase(100.0f, 100.0f), std::less<float>());
}

TEST(AnchorErodeDilateImageFilter, ClosingByReconstructionOfNearlyEqualFloats)
{
  for (const float direction : { 0.0f, 100.0f })
  {
    auto filter = itk::ClosingByReconstructionImageFilter<ImageType, ImageType, KernelType>::New();
    filter->SetInput(MakeImage(MakeUlpStaircase(0.0f, direction)));
    filter->SetKernel(KernelType::Box(KernelType::RadiusType::Filled(1)));
    EXPECT_NO_THROW(filter->Update());
  }
}
