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

#include "itkMaskedAssignImageFilter.h"
#include "itkVectorImage.h"

#include "itkTestDriverIncludeRequiredIOFactories.h"
#include "itkTestingHashImageFilter.h"

namespace
{
class MaskedAssignFixture : public ::testing::Test
{
public:
  MaskedAssignFixture() = default;
  ~MaskedAssignFixture() override = default;

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
    typename HashFilter::Pointer hasher = HashFilter::New();
    hasher->SetInput(image);
    hasher->InPlaceOff();
    hasher->Update();
    return hasher->GetHash();
  }

  template <typename TInputImage, typename TMaskImage = TInputImage>
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


    using MaskImageType = TMaskImage;
    using MaskRegionType = typename MaskImageType::RegionType;
    using MaskSizeType = typename MaskImageType::SizeType;
    using MaskIndexType = typename MaskImageType::IndexType;

    using FilterType = itk::MaskedAssignImageFilter<InputImageType, MaskImageType, OutputImageType>;

    // Create a black image or empty
    template <typename TImage>
    static typename TImage::Pointer
    CreateImageT(unsigned int size = 100)
    {
      typename TImage::Pointer image = TImage::New();

      typename TImage::SizeType imageSize;
      imageSize.Fill(size);
      image->SetRegions(typename TImage::RegionType(imageSize));
      InitializeImage(image.GetPointer());

      return image;
    }

    template <typename TPixelType, unsigned int D>
    static void
    InitializeImage(itk::Image<TPixelType, D> * image)
    {
      image->Allocate();
      image->FillBuffer(itk::NumericTraits<TPixelType>::ZeroValue());
    }


    template <typename TPixelType, unsigned int D>
    static void
    InitializeImage(itk::VectorImage<TPixelType, D> * image)
    {
      constexpr unsigned int sz = 3;
      image->SetNumberOfComponentsPerPixel(sz);
      image->Allocate();
      itk::VariableLengthVector<TPixelType> p{ sz };
      p.Fill(itk::NumericTraits<TPixelType>::ZeroValue());
      image->FillBuffer(p);
    }

    static constexpr auto CreateImage = CreateImageT<InputImageType>;
    static constexpr auto CreateMaskImage = CreateImageT<MaskImageType>;
  };
};
} // namespace


TEST_F(MaskedAssignFixture, SetGetPrint)
{
  using Utils = FixtureUtilities<itk::Image<float, 3>>;

  auto filter = Utils::FilterType::New();

  auto image = Utils::CreateImage(100);

  EXPECT_STREQ("MaskedAssignImageFilter", filter->GetNameOfClass());

  EXPECT_NO_THROW(filter->SetInput(image));
  EXPECT_EQ(image, filter->GetInput());
  EXPECT_ANY_THROW(filter->GetConstant1());

  image = Utils::CreateImage(100);
  EXPECT_NO_THROW(filter->SetInput1(image));
  EXPECT_EQ(image, filter->GetInput());
  EXPECT_ANY_THROW(filter->GetConstant1());

  EXPECT_NO_THROW(filter->SetConstant1(1));
  EXPECT_NO_THROW(filter->GetInput());
  EXPECT_EQ(1, filter->GetConstant1());


  image = Utils::CreateImage(100);
  EXPECT_NO_THROW(filter->SetInput2(image));
  EXPECT_EQ(image, filter->GetMaskImage());
  EXPECT_ANY_THROW(filter->GetConstant2());

  image = Utils::CreateImage(100);
  EXPECT_NO_THROW(filter->SetMaskImage(image));
  EXPECT_EQ(image, filter->GetMaskImage());
  EXPECT_ANY_THROW(filter->GetConstant2());

  EXPECT_NO_THROW(filter->SetConstant2(2));
  EXPECT_NO_THROW(filter->GetMaskImage());
  EXPECT_EQ(2, filter->GetConstant2());


  image = Utils::CreateImage(100);
  EXPECT_NO_THROW(filter->SetInput3(image));
  EXPECT_EQ(image, filter->GetAssignImage());
  EXPECT_ANY_THROW(filter->GetConstant3());
  EXPECT_ANY_THROW(filter->GetAssignConstant());

  image = Utils::CreateImage(100);
  EXPECT_NO_THROW(filter->SetAssignImage(image));
  EXPECT_EQ(image, filter->GetAssignImage());
  EXPECT_ANY_THROW(filter->GetConstant3());
  EXPECT_ANY_THROW(filter->GetAssignConstant());

  EXPECT_NO_THROW(filter->SetConstant3(3));
  EXPECT_NO_THROW(filter->GetMaskImage());
  EXPECT_EQ(3, filter->GetConstant3());


  EXPECT_NO_THROW(filter->SetAssignConstant(4));
  EXPECT_NO_THROW(filter->GetMaskImage());
  EXPECT_EQ(4, filter->GetConstant3());
  EXPECT_EQ(4, filter->GetAssignConstant());
}

TEST_F(MaskedAssignFixture, Test1)
{

  using Utils = FixtureUtilities<itk::Image<float, 3>, itk::Image<unsigned char, 3>>;


  auto image = Utils::CreateImage(100);
  image->FillBuffer(99);

  auto mask = Utils::CreateMaskImage(100);
  mask->FillBuffer(0);

  auto filter = Utils::FilterType::New();

  EXPECT_NO_THROW(filter->SetAssignConstant(5));
  EXPECT_EQ(5, filter->GetAssignConstant());

  mask->SetPixel({ { 0, 0 } }, 1);

  mask->SetPixel({ { 20, 21 } }, 1);

  filter->SetInput(image);
  filter->SetMaskImage(mask);

  filter->Update();
  auto output = filter->GetOutput();

  EXPECT_EQ(5, output->GetPixel({ { 0, 0 } }));
  EXPECT_EQ(99, output->GetPixel({ { 0, 1 } }));

  EXPECT_EQ(99, output->GetPixel({ { 1, 1 } }));

  EXPECT_EQ(5, output->GetPixel({ { 20, 21 } }));

  EXPECT_EQ("b28618b5cccaa828028a29b44d88c728", MD5Hash(output));
}


TEST_F(MaskedAssignFixture, VectorTest2)
{

  using Utils = FixtureUtilities<itk::VectorImage<unsigned char, 3>, itk::Image<unsigned char, 3>>;

  auto             image = Utils::CreateImage(21);
  Utils::PixelType p{ image->GetNumberOfComponentsPerPixel() };
  p.Fill(12);
  image->FillBuffer(p);

  auto filter = Utils::FilterType::New();

  Utils::PixelType c{ 3 };
  c.Fill(0);
  EXPECT_NO_THROW(filter->SetAssignConstant(c));
  EXPECT_EQ(c, filter->GetAssignConstant());


  auto mask = Utils::CreateMaskImage(21);
  mask->FillBuffer(0);
  mask->SetPixel({ { 2, 2 } }, 1);

  mask->SetPixel({ { 3, 19 } }, 1);

  filter->SetInput(image);
  filter->SetMaskImage(mask);

  filter->Update();
  auto output = filter->GetOutput();

  EXPECT_EQ(p, output->GetPixel({ { 0, 0 } }));
  EXPECT_EQ(c, output->GetPixel({ { 2, 2 } }));
  EXPECT_EQ(c, output->GetPixel({ { 3, 19 } }));


  EXPECT_EQ("f089464fcbec9429f409ee779788cca3", MD5Hash(output));
}
