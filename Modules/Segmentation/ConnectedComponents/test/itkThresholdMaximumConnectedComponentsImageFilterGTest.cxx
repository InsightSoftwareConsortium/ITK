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
#include "itkThresholdMaximumConnectedComponentsImageFilter.h"

namespace
{
itk::Image<unsigned int, 2>::Pointer
CreateDumbbellImage(unsigned int offset)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension2;

  using PixelType = unsigned int;
  using ImageType = itk::Image<PixelType, Dimension>;

  auto image = ImageType::New();
  image->SetRegions(ImageType::RegionType(itk::MakeSize(9u, 3u)));
  image->AllocateInitialized();

  for (unsigned int x = 0; x < 9; ++x)
  {
    image->SetPixel({ { x, 0 } }, offset);
    image->SetPixel({ { x, 2 } }, offset);
  }
  for (unsigned int x = 0; x < 3; ++x)
  {
    image->SetPixel({ { x, 1 } }, offset + 20);
  }
  for (unsigned int x = 3; x < 6; ++x)
  {
    image->SetPixel({ { x, 1 } }, offset + 5);
  }
  for (unsigned int x = 6; x < 9; ++x)
  {
    image->SetPixel({ { x, 1 } }, offset + 20);
  }

  return image;
}
} // namespace

TEST(ThresholdMaximumConnectedComponentsImageFilter, BisectionIsTranslationInvariant)
{
  using ImageType = itk::Image<unsigned int, 2>;
  using FilterType = itk::ThresholdMaximumConnectedComponentsImageFilter<ImageType>;

  auto filterA = FilterType::New();
  filterA->SetInput(CreateDumbbellImage(0));
  filterA->Update();

  constexpr unsigned int offset{ 3000000000u };
  auto                   filterB = FilterType::New();
  filterB->SetInput(CreateDumbbellImage(offset));
  filterB->Update();

  EXPECT_EQ(filterB->GetNumberOfObjects(), filterA->GetNumberOfObjects());
  EXPECT_EQ(filterB->GetThresholdValue(), filterA->GetThresholdValue() + offset);
}
