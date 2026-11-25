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

#include "gtest/gtest.h"

#include "itkLevelSetNeighborhoodExtractor.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace
{
using ImageType = itk::Image<float, 2>;

ImageType::Pointer
MakeGridAlignedZeroLayerImage()
{
  auto                        image = ImageType::New();
  const ImageType::SizeType   size{ { 5, 3 } };
  const ImageType::RegionType region{ size };
  image->SetRegions(region);
  image->Allocate();

  itk::ImageRegionIteratorWithIndex<ImageType> it(image, region);
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    const auto x = it.GetIndex()[0];
    it.Set(x < 2 ? -1.0f : (x == 2 ? 0.0f : 1.0f));
  }
  return image;
}
} // namespace

TEST(LevelSetNeighborhoodExtractor, SeedsOutsidePointsAcrossGridAlignedZeroContour)
{
  using ExtractorType = itk::LevelSetNeighborhoodExtractor<ImageType>;
  auto extractor = ExtractorType::New();

  extractor->SetInputLevelSet(MakeGridAlignedZeroLayerImage());
  extractor->SetLevelSetValue(0.0);
  extractor->NarrowBandingOff();
  extractor->Locate();

  const auto outsidePoints = extractor->GetOutsidePoints();
  ASSERT_EQ(outsidePoints->Size(), 3u);
  for (auto it = outsidePoints->Begin(); it != outsidePoints->End(); ++it)
  {
    EXPECT_EQ(it.Value().GetIndex()[0], 3);
    EXPECT_FLOAT_EQ(it.Value().GetValue(), 1.0f);
  }

  const auto insidePoints = extractor->GetInsidePoints();
  ASSERT_EQ(insidePoints->Size(), 6u);
  for (auto it = insidePoints->Begin(); it != insidePoints->End(); ++it)
  {
    const auto x = it.Value().GetIndex()[0];
    ASSERT_TRUE(x == 1 || x == 2);
    EXPECT_FLOAT_EQ(it.Value().GetValue(), x == 2 ? 0.0f : 1.0f);
  }
}

TEST(LevelSetNeighborhoodExtractor, SeedsInsidePointsAcrossGridAlignedZeroContour)
{
  using ExtractorType = itk::LevelSetNeighborhoodExtractor<ImageType>;
  auto extractor = ExtractorType::New();

  extractor->SetInputLevelSet(MakeGridAlignedZeroLayerImage());
  extractor->SetLevelSetValue(0.0);
  extractor->NarrowBandingOff();
  extractor->Locate();

  const auto   insidePoints = extractor->GetInsidePoints();
  unsigned int strictlyInsideSeeds = 0;
  for (auto it = insidePoints->Begin(); it != insidePoints->End(); ++it)
  {
    if (it.Value().GetIndex()[0] == 1)
    {
      ++strictlyInsideSeeds;
      EXPECT_FLOAT_EQ(it.Value().GetValue(), 1.0f);
    }
  }
  EXPECT_EQ(strictlyInsideSeeds, 3u);
}

TEST(LevelSetNeighborhoodExtractor, SeedsBothMarchesFromNarrowBandExcludingZeroPixels)
{
  using ExtractorType = itk::LevelSetNeighborhoodExtractor<ImageType>;
  using NodeContainerType = ExtractorType::NodeContainer;
  using NodeType = ExtractorType::NodeType;

  const auto image = MakeGridAlignedZeroLayerImage();

  // Band holds only the strictly-negative and strictly-positive pixels
  // bordering the zero layer; the exact-zero pixels are never visited.
  auto         band = NodeContainerType::New();
  unsigned int numberOfNodes = 0;
  for (ImageType::IndexValueType y = 0; y < 3; ++y)
  {
    for (const ImageType::IndexValueType x : { 1, 3 })
    {
      const ImageType::IndexType index{ { x, y } };
      NodeType                   node;
      node.SetIndex(index);
      node.SetValue(image->GetPixel(index));
      band->InsertElement(numberOfNodes++, node);
    }
  }

  auto extractor = ExtractorType::New();
  extractor->SetInputLevelSet(image);
  extractor->SetLevelSetValue(0.0);
  extractor->NarrowBandingOn();
  extractor->SetInputNarrowBand(band);
  extractor->Locate();

  const auto insidePoints = extractor->GetInsidePoints();
  ASSERT_EQ(insidePoints->Size(), 3u);
  for (auto it = insidePoints->Begin(); it != insidePoints->End(); ++it)
  {
    EXPECT_EQ(it.Value().GetIndex()[0], 1);
    EXPECT_FLOAT_EQ(it.Value().GetValue(), 1.0f);
  }

  const auto outsidePoints = extractor->GetOutsidePoints();
  ASSERT_EQ(outsidePoints->Size(), 3u);
  for (auto it = outsidePoints->Begin(); it != outsidePoints->End(); ++it)
  {
    EXPECT_EQ(it.Value().GetIndex()[0], 3);
    EXPECT_FLOAT_EQ(it.Value().GetValue(), 1.0f);
  }
}
