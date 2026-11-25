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
#include "itkRGBPixel.h"
#include "itkScalarToRGBColormapImageFilter.h"
#include "itkGreyColormapFunction.h"

#include <limits>

namespace
{
using ImageType = itk::Image<float, 2>;
using OutputImageType = itk::Image<itk::RGBPixel<unsigned char>, 2>;
using FilterType = itk::ScalarToRGBColormapImageFilter<ImageType, OutputImageType>;
} // namespace

TEST(ScalarToRGBColormapImageFilterGTest, NegativeFloatRangeSpansColormap)
{
  constexpr float minValue = -1000.0f;
  constexpr float maxValue = -999.0f;

  auto image = ImageType::New();
  image->SetRegions(ImageType::SizeType::Filled(2));
  image->Allocate();
  image->FillBuffer(maxValue);
  image->SetPixel(ImageType::IndexType{ { 0, 0 } }, minValue);

  auto filter = FilterType::New();
  filter->SetInput(image);
  filter->Update();

  const auto * colormap = filter->GetColormap();
  EXPECT_EQ(colormap->GetMinimumInputValue(), minValue);
  EXPECT_EQ(colormap->GetMaximumInputValue(), maxValue);

  const auto & lowOutput = filter->GetOutput()->GetPixel(ImageType::IndexType{ { 0, 0 } });
  const auto & highOutput = filter->GetOutput()->GetPixel(ImageType::IndexType{ { 1, 0 } });
  EXPECT_EQ(lowOutput[0], 0);
  EXPECT_EQ(highOutput[0], 255);
}

TEST(ScalarToRGBColormapImageFilterGTest, ColormapFunctionDefaultMinimumIsMostNegative)
{
  using ColormapType = itk::Function::GreyColormapFunction<float, OutputImageType::PixelType>;
  auto colormap = ColormapType::New();
  EXPECT_EQ(colormap->GetMinimumInputValue(), std::numeric_limits<float>::lowest());
}
