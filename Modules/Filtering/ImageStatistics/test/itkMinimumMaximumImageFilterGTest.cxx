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
#include "itkMinimumMaximumImageFilter.h"
#include "itkPipelineMonitorImageFilter.h"
#include "itkAddImageFilter.h"
#include "itkMersenneTwisterRandomVariateGenerator.h"

namespace
{

class MinimumMaximumFixture : public ::testing::Test
{
public:
  MinimumMaximumFixture() = default;
  ~MinimumMaximumFixture() override = default;

protected:
  template <unsigned int D, typename TPixelType = unsigned short>
  struct FixtureUtilities
  {
    static const unsigned int Dimension = D;
    using PixelType = TPixelType;
    using ImageType = itk::Image<PixelType, Dimension>;

    using SourceType = itk::ImageSource<ImageType>;
    using FilterType = itk::MinimumMaximumImageFilter<ImageType>;
    using MonitorFilterType = itk::PipelineMonitorImageFilter<ImageType>;
    using AddFilterType = itk::AddImageFilter<ImageType, ImageType, ImageType>;

    static typename ImageType::Pointer
    CreateImage()
    {
      // Create an image of all 1s with a random 0, and 2 valued pixel
      //

      auto image = ImageType::New();

      typename ImageType::SizeType imageSize;
      imageSize.Fill(m_ImageSize);
      typename ImageType::RegionType region(imageSize);
      image->SetRegions(region);
      image->Allocate();
      image->FillBuffer(1);

      auto randomGenerator = itk::Statistics::MersenneTwisterRandomVariateGenerator::GetInstance();

      itk::SizeValueType rand0 = randomGenerator->GetIntegerVariate(region.GetNumberOfPixels() - 1);
      itk::SizeValueType rand2 = randomGenerator->GetIntegerVariate(region.GetNumberOfPixels() - 1);

      if (rand0 == rand2)
      {
        ++rand2;
      }


      typename ImageType::IndexType idx0;
      typename ImageType::IndexType idx2;
      for (unsigned int d = 0; d < Dimension; ++d)
      {
        idx0[d] = rand0 % imageSize[d];
        idx2[d] = rand2 % imageSize[d];
        rand0 /= imageSize[d];
        rand2 /= imageSize[d];
      }
      image->SetPixel(idx0, 0);
      image->SetPixel(idx2, 2);

      return image;
    }
  };


  using Utils = FixtureUtilities<3, float>;

  void
  SetUp() override
  {
    m_Image = Utils::CreateImage();

    auto addFilter = Utils::AddFilterType::New();

    addFilter->SetInput(m_Image);
    addFilter->SetConstant2(0);
    m_Source = addFilter;
  }
  void
  TearDown() override
  {}

  Utils::SourceType *
  GetSource()
  {
    return m_Source;
  }

  Utils::ImageType *
  GetInputImage()
  {
    return m_Source->GetOutput();
  }

  Utils::ImageType::Pointer  m_Image;
  Utils::SourceType::Pointer m_Source;

  static const itk::SizeValueType m_ImageSize{ 128 };
};

} // namespace


TEST_F(MinimumMaximumFixture, test1)
{
  auto monitor = Utils::MonitorFilterType::New();
  monitor->SetInput(this->GetInputImage());

  auto filter = Utils::FilterType::New();
  filter->SetInput(monitor->GetOutput());
  filter->Update();


  EXPECT_EQ(filter->GetMinimum(), 0);
  EXPECT_EQ(filter->GetMaximum(), 2);

  EXPECT_EQ(monitor->GetNumberOfUpdates(), 1) << monitor;
}


TEST_F(MinimumMaximumFixture, test2)
{
  auto monitor = Utils::MonitorFilterType::New();
  monitor->SetInput(this->GetInputImage());

  auto filter = Utils::FilterType::New();
  filter->SetInput(monitor->GetOutput());
  filter->SetNumberOfStreamDivisions(2);
  filter->Update();


  EXPECT_EQ(filter->GetMinimum(), 0);
  EXPECT_EQ(filter->GetMaximum(), 2);

  EXPECT_TRUE(monitor->VerifyAllInputCanStream(2)) << monitor;
}


TEST_F(MinimumMaximumFixture, test3)
{
  auto monitor = Utils::MonitorFilterType::New();
  monitor->SetInput(this->GetInputImage());

  auto filter = Utils::FilterType::New();
  filter->SetInput(monitor->GetOutput());
  filter->SetNumberOfStreamDivisions(m_ImageSize);
  filter->Update();

  EXPECT_EQ(filter->GetMinimum(), 0);
  EXPECT_EQ(filter->GetMaximum(), 2);

  EXPECT_TRUE(monitor->VerifyAllInputCanStream(m_ImageSize)) << monitor;
}


TEST_F(MinimumMaximumFixture, test4)
{
  auto monitor = Utils::MonitorFilterType::New();
  monitor->SetInput(this->GetInputImage());

  auto filter = Utils::FilterType::New();
  filter->SetInput(monitor->GetOutput());
  filter->SetNumberOfStreamDivisions(m_ImageSize + 1);
  filter->Update();

  EXPECT_EQ(filter->GetMinimum(), 0);
  EXPECT_EQ(filter->GetMaximum(), 2);

  EXPECT_TRUE(monitor->VerifyAllInputCanStream(m_ImageSize)) << monitor;
}


TEST_F(MinimumMaximumFixture, test5)
{
  this->GetSource()->UpdateLargestPossibleRegion();

  auto monitor = Utils::MonitorFilterType::New();
  monitor->SetInput(this->GetInputImage());

  auto filter = Utils::FilterType::New();
  filter->SetInput(monitor->GetOutput());
  filter->SetNumberOfStreamDivisions(m_ImageSize);
  filter->Update();

  EXPECT_EQ(filter->GetMinimum(), 0);
  EXPECT_EQ(filter->GetMaximum(), 2);


  // Check when input is already updated, the pipeline doesn't get
  // updated multiple time.
  EXPECT_TRUE(monitor->VerifyInputFilterExecutedStreaming(1));
  EXPECT_TRUE(monitor->VerifyDownStreamFilterExecutedPropagation());
  EXPECT_TRUE(monitor->VerifyInputFilterMatchedUpdateOutputInformation());


  if (::testing::Test::HasFailure())
  {
    std::cout << monitor;
  }
}


TEST_F(MinimumMaximumFixture, test6)
{
  auto monitor = Utils::MonitorFilterType::New();
  monitor->SetInput(this->GetInputImage());
  monitor->ReleaseDataFlagOn();

  auto filter = Utils::FilterType::New();
  filter->SetInput(monitor->GetOutput());
  filter->SetNumberOfStreamDivisions(10);
  filter->Update();

  EXPECT_EQ(filter->GetMinimum(), 0);
  EXPECT_EQ(filter->GetMaximum(), 2);

  EXPECT_TRUE(monitor->VerifyAllInputCanStream(10)) << monitor;
}
