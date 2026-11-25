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

#include "itkLabelSetErodeImageFilter.h"
#include "itkLabelSetDilateImageFilter.h"
#include "itkImage.h"

namespace
{
using ImageType = itk::Image<unsigned char, 2>;

ImageType::Pointer
MakeFilledImage(unsigned int size, unsigned char label)
{
  auto image = ImageType::New();
  image->SetRegions(ImageType::RegionType(ImageType::SizeType::Filled(size)));
  image->Allocate();
  image->FillBuffer(label);
  return image;
}
} // namespace

TEST(LabelSetMorphBaseImageFilter, ErodeZeroRadiusOnLastAxisStillErodesActiveAxis)
{
  constexpr unsigned char label = 5;
  auto                    image = MakeFilledImage(21, 0);
  for (unsigned int i0 = 5; i0 <= 15; ++i0)
  {
    for (unsigned int i1 = 5; i1 <= 15; ++i1)
    {
      image->SetPixel(itk::MakeIndex(i0, i1), label);
    }
  }

  using FilterType = itk::LabelSetErodeImageFilter<ImageType>;
  auto filter = FilterType::New();
  filter->SetInput(image);
  filter->UseImageSpacingOn();

  FilterType::RadiusType radius;
  radius[0] = 3;
  radius[1] = 0;
  filter->SetRadius(radius);
  filter->Update();

  const ImageType * output = filter->GetOutput();
  EXPECT_EQ(output->GetPixel(itk::MakeIndex(10, 10)), label);
  EXPECT_EQ(output->GetPixel(itk::MakeIndex(5, 10)), 0);
  EXPECT_EQ(output->GetPixel(itk::MakeIndex(10, 5)), label);
}

// With UseImageSpacing() at its default of false, every axis gets a
// positive m_Scale via the "+1" margin, even at radius 0 (every axis
// erodes a little, by design). firstval must be keyed on m_Scale, not
// m_Radius: previously, with radius=[0,3], firstval picked axis 0 (the
// first nonzero *radius*) while the erosion pass for axis 0 actually ran
// with axis 0's own m_Scale — a different value from m_BaseSigma taken
// from the wrong axis. The pass's exact float comparison against
// m_BaseSigma then never matched, zeroing the entire output (issue #6575).
TEST(LabelSetMorphBaseImageFilter, ErodeWithDefaultSpacingAndZeroRadiusOnFirstAxisIsNotAllZero)
{
  constexpr unsigned char label = 5;
  auto                    image = MakeFilledImage(21, 0);
  for (unsigned int i0 = 5; i0 <= 15; ++i0)
  {
    for (unsigned int i1 = 5; i1 <= 15; ++i1)
    {
      image->SetPixel(itk::MakeIndex(i0, i1), label);
    }
  }

  using FilterType = itk::LabelSetErodeImageFilter<ImageType>;
  auto filter = FilterType::New();
  filter->SetInput(image);
  // UseImageSpacing left at its default (false).

  FilterType::RadiusType radius;
  radius[0] = 0;
  radius[1] = 3;
  filter->SetRadius(radius);
  filter->Update();

  const ImageType * output = filter->GetOutput();
  EXPECT_EQ(output->GetPixel(itk::MakeIndex(10, 10)), label)
    << "a well-interior pixel must survive; the bug zeroed the entire output";
}

TEST(LabelSetMorphBaseImageFilter, ErodeAllZeroRadiusIsIdentity)
{
  constexpr unsigned char label = 5;
  auto                    image = MakeFilledImage(5, label);
  image->SetPixel(itk::MakeIndex(0, 0), 0);

  using FilterType = itk::LabelSetErodeImageFilter<ImageType>;
  auto filter = FilterType::New();
  filter->SetInput(image);
  filter->UseImageSpacingOn();
  filter->SetRadius(0);
  filter->Update();

  const ImageType * output = filter->GetOutput();
  EXPECT_EQ(output->GetPixel(itk::MakeIndex(0, 0)), 0);
  EXPECT_EQ(output->GetPixel(itk::MakeIndex(3, 3)), label);
}

TEST(LabelSetMorphBaseImageFilter, DilateAllZeroRadiusIsIdentity)
{
  constexpr unsigned char label = 5;
  auto                    image = MakeFilledImage(5, 0);
  image->SetPixel(itk::MakeIndex(2, 2), label);

  using FilterType = itk::LabelSetDilateImageFilter<ImageType>;
  auto filter = FilterType::New();
  filter->SetInput(image);
  filter->UseImageSpacingOn();
  filter->SetRadius(0);
  filter->Update();

  const ImageType * output = filter->GetOutput();
  EXPECT_EQ(output->GetPixel(itk::MakeIndex(2, 2)), label);
  EXPECT_EQ(output->GetPixel(itk::MakeIndex(0, 0)), 0);
}
