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

#include "itkImage.h"
#include "itkImageLinearConstIteratorWithIndex.h"
#include "itkImageLinearIteratorWithIndex.h"
#include "itkImageBufferRange.h"
#include "itkGTest.h"

#include <algorithm>
#include <vector>

namespace
{
using ImageType = itk::Image<float, 3>;
using RegionType = ImageType::RegionType;

ImageType::Pointer
MakeImage()
{
  auto image = ImageType::New();
  image->SetRegions(RegionType({ { 10, 20, 30 } }, { { 6, 5, 4 } }));
  image->Allocate();
  float value = 1.0F;
  for (float & pixel : itk::ImageBufferRange<ImageType>{ *image })
  {
    pixel = value++;
  }
  return image;
}

// Visits every pixel of `region` once, in both orders, checking index, value and position agreement.
template <typename TIterator>
void
ExpectTraversal(ImageType * image, const RegionType & region, unsigned int direction)
{
  TIterator it(image, region);
  it.SetDirection(direction);
  for (const bool reverse : { false, true })
  {
    SCOPED_TRACE(reverse ? "reverse" : "forward");
    std::vector<unsigned int> visits(image->GetBufferedRegion().GetNumberOfPixels(), 0);
    reverse ? it.GoToReverseBegin() : it.GoToBegin();
    while (!(reverse ? it.IsAtReverseEnd() : it.IsAtEnd()))
    {
      while (!(reverse ? it.IsAtReverseEndOfLine() : it.IsAtEndOfLine()))
      {
        const auto index = it.GetIndex();
        ASSERT_TRUE(region.IsInside(index));
        EXPECT_EQ(it.Get(), image->GetPixel(index));
        EXPECT_EQ(visits[image->ComputeOffset(index)]++, 0U);
        reverse ? --it : ++it;
      }
      reverse ? it.PreviousLine() : it.NextLine();
    }
    EXPECT_EQ(std::count(visits.cbegin(), visits.cend(), 1U), region.GetNumberOfPixels());
  }
}

const RegionType regions[] = { RegionType({ { 10, 20, 30 } }, { { 6, 5, 4 } }),
                               RegionType({ { 11, 21, 31 } }, { { 4, 3, 2 } }),
                               RegionType({ { 10, 20, 30 } }, { { 3, 2, 2 } }),
                               RegionType({ { 13, 23, 32 } }, { { 3, 2, 2 } }) };
} // namespace

TEST(ImageLinearIteratorWithIndex, VisitsEveryPixelOnceInEveryDirection)
{
  const auto image = MakeImage();
  for (const auto & region : regions)
  {
    for (unsigned int direction = 0; direction < 3; ++direction)
    {
      SCOPED_TRACE(testing::Message() << "region " << region << " direction " << direction);
      ExpectTraversal<itk::ImageLinearConstIteratorWithIndex<ImageType>>(image, region, direction);
      ExpectTraversal<itk::ImageLinearIteratorWithIndex<ImageType>>(image, region, direction);
    }
  }
}

TEST(ImageLinearIteratorWithIndex, GoToBeginAndEndOfLineLandOnTheLine)
{
  const auto                                        image = MakeImage();
  const auto                                        region = regions[1];
  itk::ImageLinearConstIteratorWithIndex<ImageType> it(image, region);
  it.SetDirection(2);
  it.GoToBegin();
  it.NextLine();
  it.GoToEndOfLine();
  EXPECT_EQ(it.GetIndex()[2], region.GetIndex(2) + static_cast<itk::IndexValueType>(region.GetSize(2)));
  it.GoToReverseBeginOfLine();
  EXPECT_EQ(it.Get(), image->GetPixel(it.GetIndex()));
  it.GoToBeginOfLine();
  EXPECT_EQ(it.GetIndex()[2], region.GetIndex(2));
  EXPECT_EQ(it.Get(), image->GetPixel(it.GetIndex()));
}
