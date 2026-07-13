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
#include "itkImageRegionConstIterator.h"
#include "itkLabelVotingImageFilter.h"

namespace
{
using PixelType = unsigned char;
constexpr unsigned int Dimension = 2;
using ImageType = itk::Image<PixelType, Dimension>;
using FilterType = itk::LabelVotingImageFilter<ImageType>;

ImageType::Pointer
MakeImage(PixelType value)
{
  auto image = ImageType::New();
  image->SetRegions(ImageType::SizeType::Filled(2));
  image->Allocate();
  image->FillBuffer(value);
  return image;
}
} // namespace

TEST(LabelVotingImageFilterTest, ThrowsWhenUndecidedLabelDoesNotFitOutputType)
{
  auto filter = FilterType::New();
  filter->SetInput(0, MakeImage(0));
  filter->SetInput(1, MakeImage(255));

  EXPECT_THROW(filter->Update(), itk::ExceptionObject);
}

TEST(LabelVotingImageFilterTest, ExplicitUndecidedLabelOverridesOverflow)
{
  auto filter = FilterType::New();
  filter->SetInput(0, MakeImage(0));
  filter->SetInput(1, MakeImage(255));
  filter->SetLabelForUndecidedPixels(128);

  EXPECT_NO_THROW(filter->Update());
  EXPECT_EQ(128, filter->GetLabelForUndecidedPixels());

  const itk::ImageRegionConstIterator<ImageType> out(filter->GetOutput(), filter->GetOutput()->GetBufferedRegion());
  EXPECT_EQ(128, out.Get());
}

TEST(LabelVotingImageFilterTest, DefaultUndecidedLabelWhenRepresentable)
{
  auto filter = FilterType::New();
  filter->SetInput(0, MakeImage(3));
  filter->SetInput(1, MakeImage(5));

  EXPECT_NO_THROW(filter->Update());
  EXPECT_EQ(6, filter->GetLabelForUndecidedPixels());

  const itk::ImageRegionConstIterator<ImageType> out(filter->GetOutput(), filter->GetOutput()->GetBufferedRegion());
  EXPECT_EQ(6, out.Get());
}
