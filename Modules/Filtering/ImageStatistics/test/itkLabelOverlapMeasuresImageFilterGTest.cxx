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
#include "itkImageFileReader.h"
#include "itkPipelineMonitorImageFilter.h"
#include "itkLabelOverlapMeasuresImageFilter.h"

namespace
{


class LabelOverlapMeasuresImageFilterFixture : public ::testing::Test
{
public:
  LabelOverlapMeasuresImageFilterFixture() = default;
  ~LabelOverlapMeasuresImageFilterFixture() override = default;

protected:
  template <unsigned int D, typename TPixelType = unsigned short>
  struct FixtureUtilities
  {
    static const unsigned int Dimension = D;
    using PixelType = TPixelType;
    using ImageType = itk::Image<PixelType, Dimension>;
    using IndexType = typename ImageType::IndexType;

    using SourceType = itk::ImageSource<ImageType>;
    using FilterType = itk::LabelOverlapMeasuresImageFilter<ImageType>;


    static typename ImageType::Pointer
    CreateImage(PixelType fillValue = itk::NumericTraits<PixelType>::ZeroValue())
    {
      auto image = ImageType::New();

      typename ImageType::SizeType imageSize;
      imageSize.Fill(m_ImageSize);
      typename ImageType::RegionType region(imageSize);
      image->SetRegions(region);
      image->Allocate();
      image->FillBuffer(fillValue);


      return image;
    }
  };

  using Utils = FixtureUtilities<3, unsigned char>;
  static const itk::SizeValueType m_ImageSize{ 128 };
};

} // namespace

TEST_F(LabelOverlapMeasuresImageFilterFixture, test0)
{
  auto source = Utils::CreateImage(0);
  auto target = Utils::CreateImage(0);

  auto filter = Utils::FilterType::New();
  filter->SetSourceImage(source);
  filter->SetTargetImage(target);
  filter->Update();

  using RealType = Utils::FilterType::RealType;

  EXPECT_EQ(filter->GetTotalOverlap(), itk::NumericTraits<RealType>::max());
  EXPECT_EQ(filter->GetUnionOverlap(), itk::NumericTraits<RealType>::max());
  EXPECT_EQ(filter->GetMeanOverlap(), itk::NumericTraits<RealType>::infinity());
  EXPECT_EQ(filter->GetVolumeSimilarity(), itk::NumericTraits<RealType>::max());
  EXPECT_EQ(filter->GetFalseNegativeError(), itk::NumericTraits<RealType>::max());
  EXPECT_EQ(filter->GetFalsePositiveError(), itk::NumericTraits<RealType>::max());
  EXPECT_EQ(filter->GetFalseDiscoveryRate(), itk::NumericTraits<RealType>::max());
}


TEST_F(LabelOverlapMeasuresImageFilterFixture, test1)
{
  auto source = Utils::CreateImage(0);
  auto target = Utils::CreateImage(0);

  auto idx1 = itk::MakeFilled<Utils::IndexType>(1);
  source->SetPixel(idx1, 1);
  target->SetPixel(idx1, 1);

  auto filter = Utils::FilterType::New();
  filter->SetSourceImage(source);
  filter->SetTargetImage(target);
  filter->Update();

  EXPECT_NEAR(filter->GetTotalOverlap(), 1, 0.0);
  EXPECT_NEAR(filter->GetUnionOverlap(), 1, 0.0);
  EXPECT_NEAR(filter->GetMeanOverlap(), 1, 0.0);
  EXPECT_NEAR(filter->GetDiceCoefficient(), 1, 0.0);
  EXPECT_NEAR(filter->GetVolumeSimilarity(), 0, 0.0);
  EXPECT_NEAR(filter->GetFalseNegativeError(), 0, 0.0);
  EXPECT_NEAR(filter->GetFalsePositiveError(), 0, 0.0);
  EXPECT_NEAR(filter->GetFalseDiscoveryRate(), 0, 0.0);
}


TEST_F(LabelOverlapMeasuresImageFilterFixture, test2)
{
  auto source = Utils::CreateImage(0);
  auto target = Utils::CreateImage(0);

  auto idx1 = itk::MakeFilled<Utils::IndexType>(1);
  source->SetPixel(idx1, 1);
  target->SetPixel(idx1, 1);
  ++idx1[0];
  target->SetPixel(idx1, 1);

  auto filter = Utils::FilterType::New();
  filter->SetSourceImage(source);
  filter->SetTargetImage(target);
  filter->Update();

  EXPECT_NEAR(filter->GetTotalOverlap(), 0.5, 0.0);
  EXPECT_NEAR(filter->GetUnionOverlap(), 0.5, 0.0);
  EXPECT_NEAR(filter->GetMeanOverlap(), 2.0 / 3.0, 1e-15);
  EXPECT_NEAR(filter->GetDiceCoefficient(), 2.0 / 3.0, 1e-15);
  EXPECT_NEAR(filter->GetVolumeSimilarity(), -2.0 / 3.0, 1e-15);
  EXPECT_NEAR(filter->GetFalseNegativeError(), 0.5, 0.0);
  EXPECT_NEAR(filter->GetFalsePositiveError(), 0, 0.0);
  EXPECT_NEAR(filter->GetFalseDiscoveryRate(), 0, 0.0);


  EXPECT_NEAR(filter->GetTargetOverlap(1), 0.5, 0.0);
  EXPECT_NEAR(filter->GetUnionOverlap(1), 0.5, 0.0);
  EXPECT_NEAR(filter->GetMeanOverlap(1), 2.0 / 3.0, 1e-15);
  EXPECT_NEAR(filter->GetDiceCoefficient(1), 2.0 / 3.0, 1e-15);
  EXPECT_NEAR(filter->GetVolumeSimilarity(1), -2.0 / 3.0, 1e-15);
  EXPECT_NEAR(filter->GetFalseNegativeError(1), 0.5, 0.0);
  EXPECT_NEAR(filter->GetFalsePositiveError(1), 0, 0.0);
  EXPECT_NEAR(filter->GetFalseDiscoveryRate(1), 0, 0.0);
}


TEST_F(LabelOverlapMeasuresImageFilterFixture, test3)
{
  auto source = Utils::CreateImage(0);
  auto target = Utils::CreateImage(0);

  auto idx1 = itk::MakeFilled<Utils::IndexType>(1);
  source->SetPixel(idx1, 1);
  target->SetPixel(idx1, 1);
  ++idx1[0];
  target->SetPixel(idx1, 1);
  auto idx2 = itk::MakeFilled<Utils::IndexType>(3);
  source->SetPixel(idx2, 2);
  target->SetPixel(idx2, 2);
  ++idx2[2];
  source->SetPixel(idx2, 2);
  target->SetPixel(idx2, 2);
  ++idx2[2];
  source->SetPixel(idx2, 2);
  ++idx2[2];


  auto filter = Utils::FilterType::New();
  filter->SetSourceImage(source);
  filter->SetTargetImage(target);
  filter->Update();

  EXPECT_NEAR(filter->GetTotalOverlap(), 0.75, 1e-15);
  EXPECT_NEAR(filter->GetUnionOverlap(), 3.0 / 5.0, 1e-15);
  EXPECT_NEAR(filter->GetMeanOverlap(), 0.75, 1e-15);
  EXPECT_NEAR(filter->GetDiceCoefficient(), 0.75, 1e-15);
  EXPECT_NEAR(filter->GetVolumeSimilarity(), 0, 1e-15);
  EXPECT_NEAR(filter->GetFalseNegativeError(), 0.25, 1e-15);
  EXPECT_NEAR(filter->GetFalsePositiveError(), 2.38418806475e-07, 1e-17);
  EXPECT_NEAR(filter->GetFalseDiscoveryRate(), 0.25, 1e-15);


  EXPECT_NEAR(filter->GetTargetOverlap(1), 0.5, 0.0);
  EXPECT_NEAR(filter->GetUnionOverlap(1), 0.5, 0.0);
  EXPECT_NEAR(filter->GetMeanOverlap(1), 2.0 / 3.0, 1e-15);
  EXPECT_NEAR(filter->GetDiceCoefficient(1), 2.0 / 3.0, 1e-15);
  EXPECT_NEAR(filter->GetVolumeSimilarity(1), -2.0 / 3.0, 1e-15);
  EXPECT_NEAR(filter->GetFalseNegativeError(1), 0.5, 0.0);
  EXPECT_NEAR(filter->GetFalsePositiveError(1), 0, 0.0);
  EXPECT_NEAR(filter->GetFalseDiscoveryRate(1), 0, 0.0);

  EXPECT_NEAR(filter->GetTargetOverlap(2), 1.0, 0.0);
  EXPECT_NEAR(filter->GetUnionOverlap(2), 2.0 / 3.0, 2e-15);
  EXPECT_NEAR(filter->GetMeanOverlap(2), 4.0 / 5.0, 1e-15);
  EXPECT_NEAR(filter->GetDiceCoefficient(2), 4.0 / 5.0, 1e-15);
  EXPECT_NEAR(filter->GetVolumeSimilarity(2), 2.0 / 5.0, 1e-15);
  EXPECT_NEAR(filter->GetFalseNegativeError(2), 0.0, 0.0);
  EXPECT_NEAR(filter->GetFalsePositiveError(2), 4.76837612950e-07, 1e-17);
  EXPECT_NEAR(filter->GetFalseDiscoveryRate(2), 1.0 / 3.0, 0.0);
}
