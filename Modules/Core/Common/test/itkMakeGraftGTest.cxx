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

// First include the header file to be tested:
#include "itkDataObject.h"
#include "itkImage.h"
#include "itkImportImageFilter.h"
#include "itkPointSet.h"
#include <gtest/gtest.h>
#include <type_traits>
#include <vector>

namespace
{
using ImageType = itk::Image<float, 2>;
using PointSetType = itk::PointSet<float, 2>;

ImageType::Pointer
MakeImage()
{
  auto image = ImageType::New();
  image->SetRegions(ImageType::SizeType::Filled(4));
  image->SetSpacing(itk::MakeFilled<ImageType::SpacingType>(0.5));
  image->SetOrigin(itk::MakeFilled<ImageType::PointType>(-1.5));
  ImageType::DirectionType direction{};
  direction(0, 1) = 1.0;
  direction(1, 0) = -1.0;
  image->SetDirection(direction);
  image->AllocateInitialized();
  return image;
}
} // namespace


TEST(MakeGraft, ImageSharesBufferAndGeometry)
{
  const ImageType::ConstPointer image = MakeImage();
  const auto                    graft = itk::MakeGraft(image.GetPointer());

  static_assert(std::is_same_v<decltype(graft), const ImageType::Pointer>);
  ASSERT_NE(graft, nullptr);
  EXPECT_NE(graft.GetPointer(), image.GetPointer());
  EXPECT_EQ(graft->GetBufferPointer(), image->GetBufferPointer());
  EXPECT_EQ(graft->GetBufferedRegion(), image->GetBufferedRegion());
  EXPECT_EQ(graft->GetLargestPossibleRegion(), image->GetLargestPossibleRegion());
  EXPECT_EQ(graft->GetSpacing(), image->GetSpacing());
  EXPECT_EQ(graft->GetOrigin(), image->GetOrigin());
  EXPECT_EQ(graft->GetDirection(), image->GetDirection());
}


TEST(MakeGraft, ReleasingTheGraftKeepsTheOriginalBuffered)
{
  const auto image = MakeImage();
  const auto graft = itk::MakeGraft(image.GetPointer());

  graft->ReleaseData();

  EXPECT_EQ(graft->GetBufferedRegion().GetNumberOfPixels(), 0u);
  EXPECT_EQ(image->GetBufferedRegion(), image->GetLargestPossibleRegion());
  EXPECT_NE(image->GetBufferPointer(), nullptr);
}


TEST(MakeGraft, GraftOfAFilterOutputHasNoSource)
{
  std::vector<float> buffer(16, 1.0f);
  const auto         importer = itk::ImportImageFilter<float, 2>::New();
  importer->SetRegion(ImageType::RegionType(ImageType::SizeType::Filled(4)));
  importer->SetImportPointer(buffer.data(), buffer.size(), false);
  importer->Update();
  const ImageType::Pointer image = importer->GetOutput();

  const auto graft = itk::MakeGraft(image);

  ASSERT_NE(image->GetSource(), nullptr);
  EXPECT_EQ(graft->GetSource(), nullptr);
  EXPECT_EQ(graft->GetBufferPointer(), buffer.data());
}


TEST(MakeGraft, NullGivesNull)
{
  EXPECT_EQ(itk::MakeGraft(static_cast<const ImageType *>(nullptr)), nullptr);
  EXPECT_EQ(itk::MakeGraft(ImageType::ConstPointer{}), nullptr);
  EXPECT_EQ(itk::MakeGraft(ImageType::Pointer{}), nullptr);
}


TEST(MakeGraft, AcceptsSmartPointers)
{
  const ImageType::Pointer      image = MakeImage();
  const ImageType::ConstPointer constImage = image;

  static_assert(std::is_same_v<decltype(itk::MakeGraft(image)), ImageType::Pointer>);
  static_assert(std::is_same_v<decltype(itk::MakeGraft(constImage)), ImageType::Pointer>);
  EXPECT_EQ(itk::MakeGraft(image)->GetBufferPointer(), image->GetBufferPointer());
  EXPECT_EQ(itk::MakeGraft(constImage)->GetBufferPointer(), image->GetBufferPointer());
}


TEST(MakeGraft, PointSetSharesPoints)
{
  auto pointSet = PointSetType::New();
  pointSet->SetPoint(0, itk::MakeFilled<PointSetType::PointType>(1.5));

  const auto graft = itk::MakeGraft(pointSet.GetPointer());

  ASSERT_NE(graft, nullptr);
  EXPECT_EQ(graft->GetPoints(), pointSet->GetPoints());
}
