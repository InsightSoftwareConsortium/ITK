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

#include "itkFastApproximateRankImageFilter.h"
#include "itkImageRegionIteratorWithIndex.h"
#include "itkGTest.h"

namespace
{
using ImageType = itk::Image<int, 2>;
} // namespace

// SetRank() must reach the separable sub-filter of every axis, including the last
// (issue #6575, B18). Constant along axis 0, increasing along axis 1: only the
// last-axis pass changes the result, and rank 0.02 selects the window minimum.
TEST(FastApproximateRankImageFilter, RankReachesLastAxis)
{
  const ImageType::RegionType region(ImageType::SizeType::Filled(7));
  auto                        image = ImageType::New();
  image->SetRegions(region);
  image->Allocate();
  for (itk::ImageRegionIteratorWithIndex<ImageType> it(image, region); !it.IsAtEnd(); ++it)
  {
    it.Set(100 * (static_cast<int>(it.GetIndex()[1]) + 1));
  }

  using FilterType = itk::FastApproximateRankImageFilter<ImageType, ImageType>;
  auto filter = FilterType::New();
  filter->SetRadius(itk::MakeFilled<FilterType::RadiusType>(1));
  filter->SetRank(0.02F);
  ASSERT_EQ(filter->GetRank(), 0.02F);
  filter->SetInput(image);
  filter->Update();

  // Interior rows only; boundary rows have cropped windows.
  const ImageType * output = filter->GetOutput();
  for (itk::IndexValueType y = 1; y <= 5; ++y)
  {
    for (itk::IndexValueType x = 0; x <= 6; ++x)
    {
      EXPECT_EQ(output->GetPixel({ { x, y } }), 100 * y) << "at (" << x << ',' << y << ')';
    }
  }
}
