/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "itkLabelImageToStatisticsLabelMapFilter.h"
#include "itkImageRegionIterator.h"
#include <algorithm>


namespace
{

class StatisticsLabelMapFixture : public ::testing::Test
{
public:
  StatisticsLabelMapFixture() = default;
  ~StatisticsLabelMapFixture() override = default;

protected:
  void
  SetUp() override
  {}
  void
  TearDown() override
  {}

  template <unsigned int D, typename TPixelType = unsigned short>
  struct FixtureUtilities
  {
    static const unsigned int Dimension = D;

    using PixelType = TPixelType;
    using ImageType = itk::Image<PixelType, Dimension>;

    using LabelPixelType = unsigned char;
    using LabelImageType = itk::Image<LabelPixelType, Dimension>;

    using LabelObjectType = itk::StatisticsLabelObject<LabelPixelType, Dimension>;
    using StatisticsLabelMapType = itk::LabelMap<LabelObjectType>;


    static typename ImageType::Pointer
    CreateImage()
    {
      typename ImageType::Pointer image = ImageType::New();

      typename ImageType::SizeType imageSize;
      imageSize.Fill(25);
      image->SetRegions(typename ImageType::RegionType(imageSize));
      image->Allocate();
      image->FillBuffer(0);

      return image;
    }

    static typename ImageType::Pointer
    CreateImageRandom(PixelType randMax = 500, unsigned int randSeed = 0)
    {
      typename ImageType::Pointer image = ImageType::New();

      typename ImageType::SizeType imageSize;
      imageSize.Fill(25);
      image->SetRegions(typename ImageType::RegionType(imageSize));
      image->Allocate();
      image->FillBuffer(0);

      srand(randSeed);

      itk::ImageRegionIterator<ImageType> it(image, image->GetLargestPossibleRegion());
      while (!it.IsAtEnd())
      {
        it.Set(rand() % randMax);
        ++it;
      }
      return image;
    }


    static typename LabelImageType::Pointer
    CreateLabelImage()
    {
      auto image = LabelImageType::New();

      typename LabelImageType::SizeType imageSize;
      imageSize.Fill(25);
      image->SetRegions(typename ImageType::RegionType(imageSize));
      image->Allocate();
      image->FillBuffer(0);

      return image;
    }


    static typename LabelObjectType::ConstPointer
    ComputeLabelObject(const LabelImageType * labelImage,
                       const ImageType *      image,
                       const PixelType        label = 1,
                       const size_t           numberOfBins = 0)
    {

      auto l2s = itk::LabelImageToStatisticsLabelMapFilter<LabelImageType, ImageType>::New();
      l2s->SetInput1(labelImage);
      l2s->SetFeatureImage(image);
      l2s->ComputeFeretDiameterOn();
      l2s->ComputePerimeterOn();
      // l2s->ComputeOrientedBoundingBoxOn();
      l2s->ComputeHistogramOn();
      if (numberOfBins != 0)
      {
        l2s->SetNumberOfBins(numberOfBins);
      }
      l2s->Update();
      return l2s->GetOutput()->GetLabelObject(label);
    }

    static double
    ComputeExactMedian(const LabelObjectType * labelObject, const ImageType * image)
    {
      std::vector<PixelType>                       values;
      typename LabelObjectType::ConstIndexIterator it(labelObject);

      while (!it.IsAtEnd())
      {
        const typename ImageType::IndexType & idx = it.GetIndex();
        values.push_back(image->GetPixel(idx));
        ++it;
      }

      std::sort(values.begin(), values.end());

      auto n1 = values.size() / 2;
      if (values.size() % 2 == 0)
      {
        return 0.5 * (double(values[n1]) + double(values[n1 - 1]));
      }
      return values[n1];
    }
  };
};
} // namespace


TEST_F(StatisticsLabelMapFixture, 2D_zero)
{
  using Utils = FixtureUtilities<2, unsigned char>;

  auto image = Utils::CreateImage();
  auto labelImage = Utils ::CreateLabelImage();

  Utils::LabelPixelType label = 1;
  labelImage->FillBuffer(label);


  Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(labelImage, image, 1, 1 << 8);

  EXPECT_NEAR(0.0, labelObject->GetMinimum(), 1e-12);
  EXPECT_NEAR(0.0, labelObject->GetMaximum(), 1e-12);
  EXPECT_NEAR(Utils::ComputeExactMedian(labelObject, image), labelObject->GetMedian(), 1e-12);
  EXPECT_NEAR(0.0, labelObject->GetSum(), 1e-12);
  EXPECT_NEAR(0.0, labelObject->GetVariance(), 1e-12);
  EXPECT_NEAR(0.0, labelObject->GetStandardDeviation(), 1e-12);

  if (::testing::Test::HasFailure())
  {
    labelObject->Print(std::cout);
  }
}


TEST_F(StatisticsLabelMapFixture, 2D_ones_with_outliers)
{
  using Utils = FixtureUtilities<2, signed short>;

  auto             image = Utils::CreateImage();
  Utils::PixelType value = 1;
  image->FillBuffer(value);

  // Test with outliers outside the label.
  image->SetPixel({ 0, 0 }, 32000);
  image->SetPixel({ 0, 1 }, -32000);


  auto                  labelImage = Utils ::CreateLabelImage();
  Utils::LabelPixelType label = 1;
  labelImage->FillBuffer(label);
  labelImage->SetPixel({ 0, 0 }, 0);
  labelImage->SetPixel({ 0, 1 }, 0);

  Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(labelImage, image, label, 1 << 16);

  EXPECT_NEAR(value, labelObject->GetMinimum(), 1e-12);
  EXPECT_NEAR(value, labelObject->GetMaximum(), 1e-12);
  EXPECT_NEAR(Utils::ComputeExactMedian(labelObject, image), labelObject->GetMedian(), 0.5);
  EXPECT_NEAR(25 * 25 - 2, labelObject->GetSum(), 1e-12);
  EXPECT_NEAR(0.0, labelObject->GetVariance(), 1e-12);
  EXPECT_NEAR(0.0, labelObject->GetStandardDeviation(), 1e-12);

  if (::testing::Test::HasFailure())
  {
    labelObject->Print(std::cout);
  }
}

TEST_F(StatisticsLabelMapFixture, 2D_rand_with_outliers)
{
  using Utils = FixtureUtilities<2, signed short>;

  auto image = Utils::CreateImageRandom(500, 0);
  auto labelImage = Utils ::CreateLabelImage();

  // Test with outliers outside the label.
  image->SetPixel({ 0, 0 }, 32000);
  image->SetPixel({ 0, 1 }, -2000);
  // Set min/max in label
  image->SetPixel({ 0, 2 }, 0);
  image->SetPixel({ 0, 3 }, 500);

  Utils::LabelPixelType label = 1;
  labelImage->FillBuffer(label);
  labelImage->SetPixel({ 0, 0 }, 0);
  labelImage->SetPixel({ 0, 1 }, 0);


  Utils::LabelObjectType::ConstPointer labelObject = Utils::ComputeLabelObject(labelImage, image, label, 1 << 16);

  EXPECT_NEAR(0.0, labelObject->GetMinimum(), 1e-12);
  EXPECT_NEAR(500.0, labelObject->GetMaximum(), 1e-12);
  EXPECT_NEAR(Utils::ComputeExactMedian(labelObject, image), labelObject->GetMedian(), 0.5);

  if (::testing::Test::HasFailure())
  {
    labelObject->Print(std::cout);
  }
}
