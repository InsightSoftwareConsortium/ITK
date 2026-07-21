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

#include "itkFastMarchingUpwindGradientImageFilter.h"
#include "itkGTest.h"

// Duplicate entries in the target container must not make the AllTargets stop
// condition unreachable: each index is reached at most once, so the condition
// must compare against the number of distinct targets (issue #6575, B24).
TEST(FastMarchingUpwindGradientImageFilter, AllTargetsReachedWithDuplicateTargets)
{
  using ImageType = itk::Image<float, 2>;
  using FilterType = itk::FastMarchingUpwindGradientImageFilter<ImageType, ImageType>;
  using NodeType = FilterType::NodeType;
  using NodeContainer = FilterType::NodeContainer;

  constexpr auto imageSize = ImageType::SizeType::Filled(32);

  // Constant unit speed.
  auto speed = ImageType::New();
  speed->SetRegions(ImageType::RegionType{ ImageType::IndexType{}, imageSize });
  speed->Allocate();
  speed->FillBuffer(1.0F);

  // One seed at the origin.
  auto     trialPoints = NodeContainer::New();
  NodeType node;
  node.SetValue(0.0);
  node.SetIndex(ImageType::IndexType{});
  trialPoints->InsertElement(0, node);

  // Two target entries with the SAME index: only one distinct target.
  const auto targetIndex = itk::MakeIndex(10, 10);
  auto       targetPoints = NodeContainer::New();
  node.SetValue(0.0);
  node.SetIndex(targetIndex);
  targetPoints->InsertElement(0, node);
  targetPoints->InsertElement(1, node);

  auto filter = FilterType::New();
  filter->SetInput(speed);
  filter->SetOutputSize(imageSize);
  filter->SetTrialPoints(trialPoints);
  filter->SetTargetPoints(targetPoints);
  filter->SetTargetOffset(0.0);
  filter->SetStoppingValue(1000.0);
  filter->SetTargetReachedModeToAllTargets();
  filter->Update();

  // The single distinct target must be registered as reached, and the recorded
  // target arrival time must be the (positive) arrival time at that index.
  EXPECT_EQ(filter->GetReachedTargetPoints()->Size(), 1u);
  EXPECT_GT(filter->GetTargetValue(), 0.0);
  EXPECT_NEAR(filter->GetTargetValue(), static_cast<double>(filter->GetOutput()->GetPixel(targetIndex)), 1e-6);
}
