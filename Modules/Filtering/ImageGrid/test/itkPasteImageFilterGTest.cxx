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

#include "itkPasteImageFilter.h"
#include "itkVectorImage.h"

#include "itkTestDriverIncludeRequiredFactories.h"
#include "itkTestingHashImageFilter.h"

namespace
{
class PasteFixture : public ::testing::Test
{
public:
  PasteFixture() = default;
  ~PasteFixture() override = default;

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
    hasher->InPlaceOff();
    hasher->Update();
    return hasher->GetHash();
  }

  template <typename TInputImage, typename TSourceImage = TInputImage>
  struct FixtureUtilities
  {
    static const unsigned int Dimension = TInputImage::ImageDimension;

    using PixelType = typename TInputImage::PixelType;
    using OutputPixelType = PixelType;
    using InputImageType = TInputImage;
    using OutputImageType = TInputImage;
    using RegionType = typename InputImageType::RegionType;
    using SizeType = typename TInputImage::SizeType;
    using IndexType = typename TInputImage::IndexType;


    using SourceImageType = TSourceImage;
    using SourceRegionType = typename SourceImageType::RegionType;
    using SourceSizeType = typename SourceImageType::SizeType;
    using SourceIndexType = typename SourceImageType::IndexType;

    using FilterType = itk::PasteImageFilter<InputImageType, SourceImageType, OutputImageType>;

    // Create a black image or empty
    template <typename TImage>
    static typename TImage::Pointer
    CreateImageT(unsigned int size = 100)
    {
      auto image = TImage::New();

      typename TImage::SizeType imageSize;
      imageSize.Fill(size);
      image->SetRegions(typename TImage::RegionType(imageSize));
      image->Allocate();
      image->FillBuffer(0);

      return image;
    }

    static constexpr auto CreateImage = CreateImageT<InputImageType>;
    static constexpr auto CreateSourceImage = CreateImageT<SourceImageType>;
  };
};
} // namespace


TEST_F(PasteFixture, SetGetPrint)
{
  using Utils = FixtureUtilities<itk::Image<unsigned char, 3>>;

  auto filter = Utils::FilterType::New();
  filter->Print(std::cout);

  EXPECT_STREQ("PasteImageFilter", filter->GetNameOfClass());

  EXPECT_NO_THROW(filter->SetConstant(5));
  EXPECT_EQ(5, filter->GetConstant());
}


TEST_F(PasteFixture, ConstantPaste)
{
  using Utils = FixtureUtilities<itk::Image<int, 2>>;

  auto filter = Utils::FilterType::New();
  auto inputImage = Utils::CreateImage(100);

  filter->SetDestinationImage(inputImage);
  filter->SetDestinationIndex({ { 11, 13 } });

  constexpr int constantValue = -97;
  filter->SetConstant(constantValue);
  filter->SetSourceRegion(Utils::SizeType{ { 3, 3 } });

  filter->SetNumberOfWorkUnits(1);
  filter->UpdateLargestPossibleRegion();

  auto outputImage = filter->GetOutput();
  EXPECT_EQ(0, outputImage->GetPixel({ { 10, 13 } }));
  EXPECT_EQ(0, outputImage->GetPixel({ { 11, 12 } }));

  EXPECT_EQ(constantValue, outputImage->GetPixel({ { 11, 13 } }));
  EXPECT_EQ(constantValue, outputImage->GetPixel({ { 13, 15 } }));

  EXPECT_EQ("e73092e3e58f66f13d32a5aca97ca1d9", MD5Hash(filter->GetOutput()));

  filter->SetNumberOfWorkUnits(100);
  filter->UpdateLargestPossibleRegion();
  EXPECT_EQ("e73092e3e58f66f13d32a5aca97ca1d9", MD5Hash(filter->GetOutput()));


  filter->SetSourceRegion(Utils::SizeType{ { 2, 1 } });
  filter->UpdateLargestPossibleRegion();
  EXPECT_EQ("47e4689dc4a86bc6da9eeb9ce0326fa7", MD5Hash(filter->GetOutput()));


  filter->SetSourceRegion(Utils::SizeType{ { 1, 5 } });
  filter->UpdateLargestPossibleRegion();
  EXPECT_EQ("2bd084a4aa4696409b6019438ef1313f", MD5Hash(filter->GetOutput()));


  filter->SetSourceRegion(Utils::SizeType{ { 1, 3 } });
  filter->UpdateLargestPossibleRegion();
  EXPECT_EQ("07f0acc6e11ac92b5ea5dfd8ae3f9209", MD5Hash(filter->GetOutput()));


  filter->SetSourceRegion(Utils::SizeType{ { 1, 1 } });
  filter->UpdateLargestPossibleRegion();
  EXPECT_EQ("2a9bfc9d4548a986f52fb3ba2ae07de2", MD5Hash(filter->GetOutput()));
}


TEST_F(PasteFixture, ConstantPaste3_2)
{
  using Utils = FixtureUtilities<itk::Image<int, 3>, itk::Image<int, 2>>;
  using SkipType = typename Utils::FilterType::InputSkipAxesArrayType;
  auto filter = Utils::FilterType::New();


  auto inputImage = Utils::CreateImage(100);

  constexpr int constantValue = -23;
  filter->SetConstant(constantValue);


  filter->SetNumberOfWorkUnits(50);

  filter->SetDestinationImage(inputImage);

  filter->SetDestinationIndex({ { 11, 13, 17 } });
  filter->SetDestinationSkipAxes(SkipType{ { { true, false, false } } });
  filter->SetSourceRegion(Utils::SourceSizeType{ { 5, 6 } });
  filter->UpdateLargestPossibleRegion();
  EXPECT_EQ("dfdbfe702adeccece580c5e0795d8f0a", MD5Hash(filter->GetOutput()));

  EXPECT_EQ(0, filter->GetOutput()->GetPixel({ { 0, 0, 0 } }));


  filter->SetDestinationIndex({ { 11, 13, 17 } });
  filter->SetDestinationSkipAxes(SkipType{ { { false, false, true } } });
  filter->SetSourceRegion(Utils::SourceSizeType{ { 5, 6 } });
  filter->UpdateLargestPossibleRegion();
  EXPECT_EQ("7bca1328ead4ab2c6b89e1cdd1e3fdad", MD5Hash(filter->GetOutput()));

  filter->SetDestinationIndex({ { 11, 13, 17 } });
  filter->SetDestinationSkipAxes(SkipType{ { { false, false, true } } });
  filter->SetSourceRegion(Utils::SourceSizeType{ { 5, 6 } });
  filter->UpdateLargestPossibleRegion();
  EXPECT_EQ("7bca1328ead4ab2c6b89e1cdd1e3fdad", MD5Hash(filter->GetOutput()));


  filter->SetDestinationIndex({ { 11, 13, 17 } });
  filter->SetDestinationSkipAxes(SkipType{ { { true, false, false } } });
  filter->SetSourceRegion(Utils::SourceSizeType{ { 1, 1 } });
  filter->UpdateLargestPossibleRegion();
  EXPECT_EQ("2e40b486120da8d8a225d9ab505bc580", MD5Hash(filter->GetOutput()));


  filter->SetDestinationIndex({ { 11, 13, 17 } });
  filter->SetSourceRegion(Utils::SourceSizeType{ { 1, 1 } });
  filter->SetDestinationSkipAxes(SkipType{ { { true, true, true } } });
  EXPECT_THROW(filter->VerifyPreconditions(), itk::ExceptionObject);


  filter->SetDestinationSkipAxes(SkipType{ { { false, true, true } } });
  EXPECT_THROW(filter->VerifyPreconditions(), itk::ExceptionObject);


  filter->SetDestinationSkipAxes(SkipType{ { { true, true, false } } });
  EXPECT_THROW(filter->VerifyPreconditions(), itk::ExceptionObject);
}
TEST_F(PasteFixture, InPlace)
{
  // test case were source and destination are the same image.
  using Utils = FixtureUtilities<itk::Image<int, 2>>;

  auto filter = Utils::FilterType::New();
  auto inputImage = Utils::CreateImage(25);


  constexpr int constantValue = 99;
  inputImage->SetPixel(Utils::IndexType{ { 1, 1 } }, constantValue);

  filter->SetDestinationImage(inputImage);
  filter->SetSourceImage(inputImage);
  filter->InPlaceOn();

  filter->SetSourceRegion(Utils::SourceSizeType{ { 2, 2 } });
  filter->SetDestinationIndex({ { 5, 5 } });

  EXPECT_FALSE(filter->CanRunInPlace());

  filter->UpdateLargestPossibleRegion();

  auto outputImage = filter->GetOutput();

  EXPECT_EQ(constantValue, outputImage->GetPixel(Utils::IndexType{ { 1, 1 } }));
  EXPECT_EQ(constantValue, outputImage->GetPixel(Utils::IndexType{ { 6, 6 } }));

  EXPECT_EQ("0daa5bf5670eb8df504b63ddcf51d81e", MD5Hash(outputImage));
  ASSERT_NE(nullptr, inputImage->GetBufferPointer());

  auto sourceImage = Utils::CreateSourceImage(5);
  sourceImage->SetPixel(Utils::IndexType{ { 1, 3 } }, constantValue);
  filter->SetSourceImage(sourceImage);


  filter->SetSourceRegion(sourceImage->GetLargestPossibleRegion().GetSize());
  filter->SetDestinationIndex({ { 3, 3 } });

  EXPECT_TRUE(filter->CanRunInPlace());

  filter->UpdateLargestPossibleRegion();

  outputImage = filter->GetOutput();

  EXPECT_EQ(nullptr, inputImage->GetBufferPointer());
  EXPECT_EQ(constantValue, outputImage->GetPixel(Utils::IndexType{ { 4, 6 } }));
  EXPECT_EQ(0, outputImage->GetPixel(Utils::IndexType{ { 1, 3 } }));
  EXPECT_EQ("31625a3e3a34a5df00a719861d3a89da", MD5Hash(outputImage));
}


TEST_F(PasteFixture, Paste3_2)
{
  using Utils = FixtureUtilities<itk::Image<int, 3>, itk::Image<int, 2>>;

  using SkipType = typename Utils::FilterType::InputSkipAxesArrayType;
  constexpr int constantValue = -53;

  auto filter = Utils::FilterType::New();

  auto inputImage = Utils::CreateImage(25);
  filter->SetDestinationImage(inputImage);

  EXPECT_THROW(filter->VerifyPreconditions(), itk::ExceptionObject);

  auto sourceImage = Utils::CreateSourceImage(5);
  sourceImage->FillBuffer(constantValue);
  filter->SetSourceImage(sourceImage);

  filter->SetNumberOfWorkUnits(25);

  filter->SetDestinationIndex({ { 11, 13, 17 } });
  filter->SetSourceRegion(sourceImage->GetLargestPossibleRegion());

  filter->SetDestinationSkipAxes(SkipType{ { { true, false, false } } });
  filter->UpdateLargestPossibleRegion();
  EXPECT_EQ("753e433a43ab8fcf3d2ef0f8c78aef35", MD5Hash(filter->GetOutput()));
  EXPECT_EQ(0, filter->GetOutput()->GetPixel({ { 12, 13, 17 } }));

  filter->SetDestinationSkipAxes(SkipType{ { { false, true, false } } });
  filter->UpdateLargestPossibleRegion();
  EXPECT_EQ(0, filter->GetOutput()->GetPixel({ { 11, 14, 17 } }));
  EXPECT_EQ("44bd0a10b89c58fd306beee6148fdb4d", MD5Hash(filter->GetOutput()));


  filter->SetDestinationSkipAxes(SkipType{ { { false, false, true } } });
  filter->UpdateLargestPossibleRegion();
  EXPECT_EQ("ce630d54304b6eba8cd73ec9617d2cf4", MD5Hash(filter->GetOutput()));
  EXPECT_EQ(0, filter->GetOutput()->GetPixel({ { 11, 13, 18 } }));
}
