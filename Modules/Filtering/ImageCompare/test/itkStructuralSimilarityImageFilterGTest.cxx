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
#include "itkStructuralSimilarityImageFilter.h"
namespace
{
using PixelType = float;
constexpr unsigned int Dimension = 2;
using ImageType = itk::Image<PixelType, Dimension>;
using FilterType = itk::StructuralSimilarityImageFilter<ImageType>;
ImageType::Pointer
CreateConstantImage(PixelType value, unsigned int size = 32)
{
  auto image = ImageType::New();
  ImageType::SizeType imageSize;
  imageSize.Fill(size);
  const ImageType::RegionType region(imageSize);
  image->SetRegions(region);
  image->Allocate();
  image->FillBuffer(value);
  return image;
}
ImageType::Pointer
CreateGradientImage(unsigned int size = 32)
{
  auto image = ImageType::New();
  ImageType::SizeType imageSize;
  imageSize.Fill(size);
  const ImageType::RegionType region(imageSize);
  image->SetRegions(region);
  image->Allocate();
  itk::ImageRegionIteratorWithIndex<ImageType> it(image, region);
  for (it.GoToBegin(); !it.IsAtEnd(); ++it)
  {
    const auto idx = it.GetIndex();
    it.Set(static_cast<PixelType>(idx[0] + idx[1]));
  }
  return image;
}
} // namespace
TEST(StructuralSimilarityImageFilter, BasicObjectMethods)
{
  auto filter = FilterType::New();
  ITK_GTEST_EXERCISE_BASIC_OBJECT_METHODS(filter, StructuralSimilarityImageFilter, ImageToImageFilter);
}
TEST(StructuralSimilarityImageFilter, IdenticalImagesGiveSSIMOfOne)
{
  auto image = CreateConstantImage(100.0f);
  auto filter = FilterType::New();
  filter->SetInput1(image);
  filter->SetInput2(image);
  filter->Update();
  EXPECT_NEAR(filter->GetSSIM(), 1.0, 1e-6);
}
TEST(StructuralSimilarityImageFilter, IdenticalGradientImagesGiveSSIMOfOne)
{
  auto image = CreateGradientImage();
  auto filter = FilterType::New();
  filter->SetInput1(image);
  filter->SetInput2(image);
  filter->Update();
  EXPECT_NEAR(filter->GetSSIM(), 1.0, 1e-6);
}
TEST(StructuralSimilarityImageFilter, DifferentConstantImagesHaveLowSSIM)
{
  auto image1 = CreateConstantImage(0.0f);
  auto image2 = CreateConstantImage(255.0f);
  auto filter = FilterType::New();
  filter->SetInput1(image1);
  filter->SetInput2(image2);
  filter->Update();
  // Two maximally different constant images should have a very low SSIM
  EXPECT_LT(filter->GetSSIM(), 0.01);
}
TEST(StructuralSimilarityImageFilter, OutputMapHasCorrectSize)
{
  constexpr unsigned int imageSize = 16;
  auto                   image1 = CreateConstantImage(100.0f, imageSize);
  auto                   image2 = CreateConstantImage(100.0f, imageSize);
  auto filter = FilterType::New();
  filter->SetInput1(image1);
  filter->SetInput2(image2);
  filter->Update();
  auto output = filter->GetOutput();
  EXPECT_EQ(output->GetLargestPossibleRegion().GetSize()[0], imageSize);
  EXPECT_EQ(output->GetLargestPossibleRegion().GetSize()[1], imageSize);
}
TEST(StructuralSimilarityImageFilter, SetGetParameters)
{
  auto filter = FilterType::New();
  filter->SetLuminanceWeight(2.0);
  EXPECT_DOUBLE_EQ(filter->GetLuminanceWeight(), 2.0);
  filter->SetContrastWeight(3.0);
  EXPECT_DOUBLE_EQ(filter->GetContrastWeight(), 3.0);
  filter->SetStructureWeight(0.5);
  EXPECT_DOUBLE_EQ(filter->GetStructureWeight(), 0.5);
  filter->SetDynamicRange(1.0);
  EXPECT_DOUBLE_EQ(filter->GetDynamicRange(), 1.0);
  filter->SetK1(0.05);
  EXPECT_DOUBLE_EQ(filter->GetK1(), 0.05);
  filter->SetK2(0.1);
  EXPECT_DOUBLE_EQ(filter->GetK2(), 0.1);
  FilterType::RadiusType radius;
  radius.Fill(3);
  filter->SetRadius(radius);
  EXPECT_EQ(filter->GetRadius()[0], 3u);
  EXPECT_EQ(filter->GetRadius()[1], 3u);
  filter->SetRadius(5u);
  EXPECT_EQ(filter->GetRadius()[0], 5u);
  EXPECT_EQ(filter->GetRadius()[1], 5u);
}
TEST(StructuralSimilarityImageFilter, ScalarRadiusSetGet)
{
  auto filter = FilterType::New();
  filter->SetRadius(4u);
  for (unsigned int d = 0; d < Dimension; ++d)
  {
    EXPECT_EQ(filter->GetRadius()[d], 4u);
  }
}
TEST(StructuralSimilarityImageFilter, SlightDifferenceGivesHighSSIM)
{
  auto image1 = CreateGradientImage(32);
  auto image2 = CreateGradientImage(32);
  // Perturb one pixel slightly
  ImageType::IndexType idx;
  idx.Fill(16);
  image2->SetPixel(idx, image2->GetPixel(idx) + 1.0f);
  auto filter = FilterType::New();
  filter->SetInput1(image1);
  filter->SetInput2(image2);
  filter->Update();
  // A single-pixel perturbation in a gradient image should still yield high SSIM
  EXPECT_GT(filter->GetSSIM(), 0.99);
}
TEST(StructuralSimilarityImageFilter, LargerRadiusWorks)
{
  auto image = CreateGradientImage(32);
  auto filter = FilterType::New();
  filter->SetInput1(image);
  filter->SetInput2(image);
  filter->SetRadius(3u);
  filter->Update();
  EXPECT_NEAR(filter->GetSSIM(), 1.0, 1e-6);
}
TEST(StructuralSimilarityImageFilter, CustomWeightsWork)
{
  auto image1 = CreateGradientImage(32);
  auto image2 = CreateGradientImage(32);
  // Add noise
  ImageType::IndexType idx;
  idx[0] = 10;
  idx[1] = 10;
  image2->SetPixel(idx, image2->GetPixel(idx) + 50.0f);
  auto filter = FilterType::New();
  filter->SetInput1(image1);
  filter->SetInput2(image2);
  filter->SetLuminanceWeight(1.0);
  filter->SetContrastWeight(1.0);
  filter->SetStructureWeight(1.0);
  filter->Update();
  // Should produce a valid SSIM between 0 and 1
  EXPECT_GT(filter->GetSSIM(), 0.0);
  EXPECT_LE(filter->GetSSIM(), 1.0);
}
TEST(StructuralSimilarityImageFilter, ThreeDimensionalImage)
{
  using Image3DType = itk::Image<float, 3>;
  using Filter3DType = itk::StructuralSimilarityImageFilter<Image3DType>;
  auto image = Image3DType::New();
  Image3DType::SizeType size;
  size.Fill(8);
  image->SetRegions(Image3DType::RegionType(size));
  image->Allocate();
  image->FillBuffer(42.0f);
  auto filter = Filter3DType::New();
  filter->SetInput1(image);
  filter->SetInput2(image);
  filter->Update();
  EXPECT_NEAR(filter->GetSSIM(), 1.0, 1e-6);
}
