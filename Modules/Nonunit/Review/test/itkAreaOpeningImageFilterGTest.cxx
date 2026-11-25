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

#include "itkAreaOpeningImageFilter.h"
#include "itkGTest.h"

#include <array>

namespace
{
using ImageType = itk::Image<int, 2>;

std::array<int, 5>
AreaOpenRow(const std::array<int, 5> & values)
{
  auto image = ImageType::New();
  image->SetRegions(ImageType::RegionType{ ImageType::SizeType{ 5, 1 } });
  image->Allocate();
  for (int i = 0; i < 5; ++i)
  {
    image->SetPixel({ { i, 0 } }, values[i]);
  }

  auto filter = itk::AreaOpeningImageFilter<ImageType, ImageType>::New();
  filter->SetInput(image);
  filter->UseImageSpacingOff();
  // Lambda = 2 removes every component smaller than 2 pixels.
  filter->SetLambda(2.0);
  filter->Update();

  std::array<int, 5> result{};
  for (int i = 0; i < 5; ++i)
  {
    result[i] = filter->GetOutput()->GetPixel({ { i, 0 } });
  }
  return result;
}
} // namespace

// The pixel-buffer sort must include the last raster pixel (issue #6575, B19).
TEST(AreaOpeningImageFilter, RemovesSinglePixelPeakAtLastRasterPosition)
{
  const std::array<int, 5> expected{ 0, 0, 0, 0, 0 };
  EXPECT_EQ(AreaOpenRow({ 0, 0, 0, 0, 5 }), expected);
}

// Control: the identical area-1 peak mid-row is removed with or without the fix.
TEST(AreaOpeningImageFilter, RemovesSinglePixelPeakMidRow)
{
  const std::array<int, 5> expected{ 0, 0, 0, 0, 0 };
  EXPECT_EQ(AreaOpenRow({ 0, 0, 5, 0, 0 }), expected);
}
