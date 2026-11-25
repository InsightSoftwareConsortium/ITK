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

#include "itkLabelMap.h"
#include "itkLabelObject.h"
#include "itkStatisticsLabelObject.h"
#include "itkStatisticsLabelMapFilter.h"
#include "itkLabelUniqueLabelMapFilter.h"
#include "itkGTest.h"

// A completely full label range must make PushLabelObject throw, not silently
// overwrite an existing entry (issue #6575, B44).
TEST(LabelMap, PushLabelObjectThrowsWhenFull)
{
  using LabelObjectType = itk::LabelObject<unsigned char, 2>;
  using LabelMapType = itk::LabelMap<LabelObjectType>;

  auto labelMap = LabelMapType::New();
  labelMap->SetRegions(LabelMapType::RegionType{ LabelMapType::IndexType{}, LabelMapType::SizeType::Filled(4) });

  // Background is 0; occupy every remaining label 1..255.
  for (unsigned int label = 1; label <= 255; ++label)
  {
    labelMap->SetLine(LabelMapType::IndexType{}, 1, static_cast<unsigned char>(label));
  }
  ASSERT_EQ(labelMap->GetNumberOfLabelObjects(), 255u);

  auto extra = LabelObjectType::New();
  extra->AddLine(itk::MakeIndex(0, 1), 1);
  EXPECT_THROW(labelMap->PushLabelObject(extra), itk::ExceptionObject);
}

// Weighted elongation/flatness must stay finite when a signed feature image makes
// the weighted covariance indefinite (issue #6575, B45).
TEST(StatisticsLabelMapFilter, WeightedShapeFinelyDefinedForSignedFeature)
{
  using LabelObjectType = itk::StatisticsLabelObject<unsigned char, 2>;
  using LabelMapType = itk::LabelMap<LabelObjectType>;
  using FeatureImageType = itk::Image<float, 2>;

  auto labelMap = LabelMapType::New();
  labelMap->SetRegions(LabelMapType::RegionType{ LabelMapType::IndexType{}, LabelMapType::SizeType::Filled(9) });
  // A cross: horizontal arm (y == 4, x in 2..6) and vertical arm (x == 4, y in 2,3,5,6).
  labelMap->SetLine(itk::MakeIndex(2, 4), 5, 1);
  labelMap->SetLine(itk::MakeIndex(4, 2), 1, 1);
  labelMap->SetLine(itk::MakeIndex(4, 3), 1, 1);
  labelMap->SetLine(itk::MakeIndex(4, 5), 1, 1);
  labelMap->SetLine(itk::MakeIndex(4, 6), 1, 1);

  // Positive weights along the x arm, negative along the y arm: the weighted
  // second moment is positive in xx and negative in yy -> indefinite covariance.
  auto feature = FeatureImageType::New();
  feature->SetRegions(labelMap->GetLargestPossibleRegion());
  feature->Allocate();
  feature->FillBuffer(0.0F);
  for (int x = 2; x <= 6; ++x)
  {
    feature->SetPixel(itk::MakeIndex(x, 4), 2.0F);
  }
  for (int y : { 2, 3, 5, 6 })
  {
    feature->SetPixel(itk::MakeIndex(4, y), -1.0F);
  }

  auto filter = itk::StatisticsLabelMapFilter<LabelMapType, FeatureImageType>::New();
  filter->SetInput(labelMap);
  filter->SetFeatureImage(feature);
  filter->Update();

  const auto * object = filter->GetOutput()->GetLabelObject(1);
  EXPECT_TRUE(std::isfinite(object->GetWeightedElongation())) << " weighted elongation";
  EXPECT_TRUE(std::isfinite(object->GetWeightedFlatness())) << " weighted flatness";
}

// A label object whose every pixel is claimed by a higher-priority object becomes
// empty and must be removed; the removal pass used an inverted loop condition and
// never ran (issue #6575, B43).
TEST(LabelUniqueLabelMapFilter, EmptiedObjectIsRemoved)
{
  using LabelObjectType = itk::LabelObject<unsigned char, 2>;
  using LabelMapType = itk::LabelMap<LabelObjectType>;

  auto labelMap = LabelMapType::New();
  labelMap->SetRegions(LabelMapType::RegionType{ LabelMapType::IndexType{}, LabelMapType::SizeType::Filled(8) });
  // Two objects covering exactly the same pixels: after uniqueness one is emptied.
  labelMap->SetLine(itk::MakeIndex(0, 0), 4, 1);
  labelMap->SetLine(itk::MakeIndex(0, 0), 4, 2);
  ASSERT_EQ(labelMap->GetNumberOfLabelObjects(), 2u);

  auto unique = itk::LabelUniqueLabelMapFilter<LabelMapType>::New();
  unique->SetInput(labelMap);
  unique->Update();

  EXPECT_EQ(unique->GetOutput()->GetNumberOfLabelObjects(), 1u);
}
