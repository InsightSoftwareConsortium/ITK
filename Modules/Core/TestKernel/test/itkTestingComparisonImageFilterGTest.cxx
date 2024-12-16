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

#include "itkTestingComparisonImageFilter.h"
#include "itkImage.h"
#include "itkVectorImage.h"

#include "itkGTest.h"


// A Google test to check the class's run time type information
TEST(itkTestingComparisonImageFilterTest, CheckRunTimeTypeInformation)
{
  // Create an image
  using ImageType = itk::Image<unsigned char, 2>;
  auto filter = itk::Testing::ComparisonImageFilter<ImageType, ImageType>::New();

  // Test the run-time type information
  EXPECT_STREQ(filter->GetNameOfClass(), "ComparisonImageFilter");
  EXPECT_NO_THROW(filter->Print(std::cout));
}

// Test two 16x16 images of all zeros, and check all statistics are zero
TEST(itkTestingComparisonImageFilterTest, TestZeroImages)
{
  // Create two 16x16 images of all zeros
  using ImageType = itk::Image<unsigned short, 2>;
  auto                          image1 = ImageType::New();
  auto                          image2 = ImageType::New();
  constexpr ImageType::SizeType size = { { 16, 16 } };
  image1->SetRegions(size);
  image2->SetRegions(size);
  image1->AllocateInitialized();
  image2->AllocateInitialized();
  image1->FillBuffer(0);
  image2->FillBuffer(0);

  // Create the filter
  auto filter = itk::Testing::ComparisonImageFilter<ImageType, ImageType>::New();
  filter->SetValidInput(image1);
  filter->SetTestInput(image2);
  filter->Update();

  // Check all statistics are zero
  EXPECT_EQ(filter->GetNumberOfPixelsWithDifferences(), 0);
  // EXPECT_EQ(filter->GetMinimumDifference(), 0);
  // because no pixel has a difference the minimum is not updated
  EXPECT_EQ(filter->GetMaximumDifference(), 0);
  EXPECT_EQ(filter->GetMeanDifference(), 0);
  EXPECT_EQ(filter->GetTotalDifference(), 0);
}

// Test two 16x16 images of all zeros but pixel at (5,5) is 1, and check all statistics
TEST(itkTestingComparisonImageFilterTest, TestOneDifferentPixel)
{
  // Create two 16x16 images of all zeros but pixel at (5,5) is 1
  using ImageType = itk::Image<unsigned char, 2>;
  auto                          image1 = ImageType::New();
  auto                          image2 = ImageType::New();
  constexpr ImageType::SizeType size = { { 16, 16 } };
  image1->SetRegions(size);
  image2->SetRegions(size);
  image1->AllocateInitialized();
  image2->AllocateInitialized();
  image1->SetPixel({ { 5, 5 } }, 1);

  image2->SetPixel({ { 5, 6 } }, 1);

  // Create the filter
  auto filter = itk::Testing::ComparisonImageFilter<ImageType, ImageType>::New();
  filter->SetValidInput(image1);
  filter->SetTestInput(image2);
  filter->Update();

  // Check all statistics
  EXPECT_EQ(filter->GetNumberOfPixelsWithDifferences(), 2);
  EXPECT_EQ(filter->GetMinimumDifference(), 1);
  EXPECT_EQ(filter->GetMaximumDifference(), 1);
  EXPECT_EQ(filter->GetMeanDifference(), 1);
  EXPECT_EQ(filter->GetTotalDifference(), 2);

  // then check with a radius of 1
  filter->SetToleranceRadius(2);
  filter->Update();
  EXPECT_EQ(filter->GetNumberOfPixelsWithDifferences(), 0);
  //  EXPECT_EQ(filter->GetMinimumDifference(), 0);
  // because no pixel has a difference the minimum is not updated
  EXPECT_EQ(filter->GetMaximumDifference(), 0);
  EXPECT_EQ(filter->GetMeanDifference(), 0);
  EXPECT_EQ(filter->GetTotalDifference(), 0);
}
