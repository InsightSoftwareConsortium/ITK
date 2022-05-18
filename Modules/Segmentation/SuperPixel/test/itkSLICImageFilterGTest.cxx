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

#include "itkSLICImageFilter.h"
#include "itkVectorImage.h"

#include "itkCommand.h"

#include "itkTestDriverIncludeRequiredFactories.h"
#include "itkTestingHashImageFilter.h"

namespace
{

class SLICFixture : public ::testing::Test
{
public:
  SLICFixture() = default;
  ~SLICFixture() override = default;

protected:
  void
  SetUp() override
  {
    RegisterRequiredFactories();
  }


  template <typename TImageType>
  static std::string
  MD5Hash(const TImageType * image)
  {

    using HashFilter = itk::Testing::HashImageFilter<TImageType>;
    auto hasher = HashFilter::New();
    hasher->SetInput(image);
    hasher->Update();
    return hasher->GetHash();
  }

  template <unsigned int D, typename TPixelType = unsigned short>
  struct FixtureUtilities
  {
    static const unsigned int Dimension = D;

    using PixelType = TPixelType;
    using OutputPixelType = unsigned int;
    using InputImageType = itk::Image<PixelType, Dimension>;
    using OutputImageType = itk::Image<OutputPixelType, Dimension>;

    using FilterType = itk::SLICImageFilter<InputImageType, OutputImageType>;

    // Create a black image or empty
    static typename InputImageType::Pointer
    CreateImage(unsigned int size = 100)
    {
      auto image = InputImageType::New();

      typename InputImageType::SizeType imageSize;
      imageSize.Fill(size);
      image->SetRegions(typename InputImageType::RegionType(imageSize));
      image->Allocate();
      image->FillBuffer(0);

      return image;
    }
  };
};
} // namespace


TEST_F(SLICFixture, SetGetPrint)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension3;
  using Utils = FixtureUtilities<3>;

  auto filter = Utils::FilterType::New();
  filter->Print(std::cout);

  typename Utils::FilterType::ConstPointer constfilter = (const Utils::FilterType *)(filter.GetPointer());

  EXPECT_STREQ("SLICImageFilter", filter->GetNameOfClass());
  EXPECT_STREQ("ImageToImageFilter", filter->Superclass::GetNameOfClass());

  Utils::FilterType::SuperGridSizeType gridSize3(3);
  EXPECT_NO_THROW(filter->SetSuperGridSize(gridSize3));
  ITK_EXPECT_VECTOR_NEAR(gridSize3, filter->GetSuperGridSize(), 0);

  EXPECT_NO_THROW(filter->SetSuperGridSize(4));
  ITK_EXPECT_VECTOR_NEAR(Utils::FilterType::SuperGridSizeType(4), filter->GetSuperGridSize(), 0);

  EXPECT_NO_THROW(filter->SetMaximumNumberOfIterations(6));
  EXPECT_EQ(6, filter->GetMaximumNumberOfIterations());

  EXPECT_NO_THROW(filter->SetSpatialProximityWeight(9.1));
  EXPECT_EQ(9.1, filter->GetSpatialProximityWeight());

  EXPECT_NO_THROW(filter->EnforceConnectivityOn());
  EXPECT_TRUE(filter->GetEnforceConnectivity());
  EXPECT_NO_THROW(filter->EnforceConnectivityOff());
  EXPECT_FALSE(filter->GetEnforceConnectivity());

  EXPECT_NO_THROW(filter->SetEnforceConnectivity(true));
  EXPECT_TRUE(filter->GetEnforceConnectivity());


  EXPECT_NO_THROW(filter->InitializationPerturbationOn());
  EXPECT_TRUE(filter->GetInitializationPerturbation());
  EXPECT_NO_THROW(filter->InitializationPerturbationOff());
  EXPECT_FALSE(filter->GetInitializationPerturbation());

  EXPECT_NO_THROW(filter->SetInitializationPerturbation(true));
  EXPECT_TRUE(filter->GetInitializationPerturbation());
}

TEST_F(SLICFixture, Blank2DImage)
{

  using namespace itk::GTest::TypedefsAndConstructors::Dimension2;
  using Utils = FixtureUtilities<2>;

  auto filter = Utils::FilterType::New();

  auto image = Utils::CreateImage(100);
  filter->SetInput(image);

  filter->SetSuperGridSize(10);
  filter->Update();
  EXPECT_EQ("68707adc3df2f7d210b1db96847fc3c5", MD5Hash(filter->GetOutput()));


  filter->SetSuperGridSize(1);
  filter->Update();
  EXPECT_EQ("10d461742d48d15b8df75387187de426", MD5Hash(filter->GetOutput()));

  filter->SetSuperGridSize(200);
  filter->Update();
  EXPECT_EQ("4e0a293a5b638f0aba2c4fe2c3418d0e", MD5Hash(filter->GetOutput()));
}


TEST_F(SLICFixture, ClusterInitializationOverflow)
{
  // Tests a case failure caused by numeric overflow during initialization of clusters.
  using namespace itk::GTest::TypedefsAndConstructors::Dimension2;
  using Utils = FixtureUtilities<2, unsigned char>;

  auto filter = Utils::FilterType::New();

  auto image = Utils::CreateImage(100);

  image->FillBuffer(255);
  for (unsigned int x = 2; x < 5; ++x)
  {
    for (unsigned int y = 2; y < 5; ++y)
    {
      image->SetPixel(itk::MakeIndex(x, y), 254);
    }
  }
  filter->SetInput(image);
  filter->SetMaximumNumberOfIterations(1);

  filter->SetSuperGridSize(10);
  filter->Update();
  EXPECT_EQ("be2250b1d36e8a418f6487189db1ea64", MD5Hash(filter->GetOutput()));
  EXPECT_FLOAT_EQ(0.023752308, filter->GetAverageResidual());
}
