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

#include "itkFastMarchingImageFilterBase.h"
#include "itkFastMarchingThresholdStoppingCriterion.h"
#include "gtest/gtest.h"

namespace
{
using ImageType = itk::Image<float, 2>;
using FastMarchingType = itk::FastMarchingImageFilterBase<ImageType, ImageType>;
using CriterionType = itk::FastMarchingThresholdStoppingCriterion<ImageType, ImageType>;
using NodePairType = FastMarchingType::NodePairType;
using NodePairContainerType = FastMarchingType::NodePairContainerType;
} // namespace

TEST(FastMarchingImageFilterBaseGTest, CornerSeedPropagatesAlongBothBoundaryAxes)
{
  auto marcher = FastMarchingType::New();

  constexpr ImageType::SizeType size{ { 5, 5 } };
  marcher->SetOutputSize(size);

  auto criterion = CriterionType::New();
  criterion->SetThreshold(100.0);
  marcher->SetStoppingCriterion(criterion);

  auto trial = NodePairContainerType::New();
  trial->push_back(NodePairType(itk::MakeIndex(0, 0), 0.0));
  marcher->SetTrialPoints(trial);

  marcher->Update();

  const auto * output = marcher->GetOutput();
  EXPECT_LT(output->GetPixel(itk::MakeIndex(1, 0)), 100.0f);
  EXPECT_LT(output->GetPixel(itk::MakeIndex(0, 1)), 100.0f);
}

TEST(FastMarchingImageFilterBaseGTest, NoHandlesAllowsMergeOfSeparatelyMarchedRegions)
{
  auto marcher = FastMarchingType::New();

  constexpr ImageType::SizeType size{ { 5, 1 } };
  marcher->SetOutputSize(size);
  marcher->SetTopologyCheck(FastMarchingType::TopologyCheckEnum::NoHandles);

  auto criterion = CriterionType::New();
  criterion->SetThreshold(100.0);
  marcher->SetStoppingCriterion(criterion);

  auto trial = NodePairContainerType::New();
  trial->push_back(NodePairType(itk::MakeIndex(0, 0), 0.0));
  trial->push_back(NodePairType(itk::MakeIndex(4, 0), 0.0));
  marcher->SetTrialPoints(trial);

  marcher->Update();

  EXPECT_EQ(marcher->GetLabelImage()->GetPixel(itk::MakeIndex(2, 0)), FastMarchingType::Traits::Alive);
}

TEST(FastMarchingImageFilterBaseGTest, ZeroSpeedPixelStaysUnreachable)
{
  auto marcher = FastMarchingType::New();

  constexpr ImageType::SizeType size{ { 2, 1 } };

  auto speedImage = ImageType::New();
  speedImage->SetRegions(size);
  speedImage->Allocate();
  speedImage->FillBuffer(1.0f);
  speedImage->SetPixel(itk::MakeIndex(1, 0), 0.0f);
  marcher->SetInput(speedImage);

  auto criterion = CriterionType::New();
  criterion->SetThreshold(1e30);
  marcher->SetStoppingCriterion(criterion);

  auto trial = NodePairContainerType::New();
  trial->push_back(NodePairType(itk::MakeIndex(0, 0), 0.0));
  marcher->SetTrialPoints(trial);

  marcher->Update();

  EXPECT_EQ(marcher->GetLabelImage()->GetPixel(itk::MakeIndex(1, 0)), FastMarchingType::Traits::Far);
}
