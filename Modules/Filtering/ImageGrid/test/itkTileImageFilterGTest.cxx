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

#include "itkTileImageFilter.h"
#include "itkVectorImage.h"


#include "itkTestDriverIncludeRequiredIOFactories.h"
#include "itkTestingHashImageFilter.h"


namespace
{

class TileImageFixture : public ::testing::Test
{
public:
  TileImageFixture() = default;
  ~TileImageFixture() override = default;

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
    hasher->Update();
    return hasher->GetHash();
  }

  template <typename TImage>
  struct FixtureUtilities
  {
    static const unsigned int Dimension = TImage::ImageDimension;

    using PixelType = typename TImage::PixelType;
    using OutputPixelType = PixelType;
    using InputImageType = TImage;
    using OutputImageType = TImage;
    using RegionType = typename InputImageType::RegionType;

    using FilterType = itk::TileImageFilter<InputImageType, OutputImageType>;

    // Create a black image or empty
    static typename InputImageType::Pointer
    CreateImage(unsigned int size = 100)
    {
      typename InputImageType::Pointer image = InputImageType::New();

      typename InputImageType::SizeType imageSize;
      imageSize.Fill(size);
      image->SetRegions(RegionType(imageSize));
      image->Allocate();
      image->FillBuffer(0);

      return image;
    }
  };
};
} // namespace


TEST_F(TileImageFixture, SetGetPrint)
{
  using Utils = FixtureUtilities<itk::Image<unsigned char, 3>>;

  auto filter = Utils::FilterType::New();
  filter->Print(std::cout);

  typename Utils::FilterType::ConstPointer constfilter = (const Utils::FilterType *)(filter.GetPointer());

  EXPECT_STREQ("TileImageFilter", filter->GetNameOfClass());
  EXPECT_STREQ("ImageToImageFilter", filter->Superclass::GetNameOfClass());

  Utils::FilterType::LayoutArrayType layout(99);

  EXPECT_NO_THROW(filter->SetLayout(layout));
  ITK_EXPECT_VECTOR_NEAR(layout, filter->GetLayout(), 0);


  EXPECT_NO_THROW(filter->SetDefaultPixelValue(99));
  EXPECT_EQ(99, filter->GetDefaultPixelValue());
}


TEST_F(TileImageFixture, VectorImage)
{
  using namespace itk::GTest::TypedefsAndConstructors::Dimension2;

  using ImageType = itk::VectorImage<unsigned char, 2>;


  auto image = ImageType::New();

  typename ImageType::SizeType imageSize = MakeSize(10, 10);

  const unsigned int numberOfComponents = 5;

  ImageType::PixelType v(numberOfComponents);

  v[0] = 2;
  v[1] = 3;
  v[2] = 5;
  v[3] = 7;
  v[4] = 11;


  image->SetRegions(ImageType::RegionType(imageSize));
  image->SetNumberOfComponentsPerPixel(numberOfComponents);

  image->Allocate();
  image->FillBuffer(v);


  using FilterType = itk::TileImageFilter<ImageType, ImageType>;
  auto filter = FilterType::New();

  FilterType::LayoutArrayType layout(2);

  EXPECT_NO_THROW(filter->SetLayout(layout));
  ITK_EXPECT_VECTOR_NEAR(layout, filter->GetLayout(), 0);

  filter->SetInput(0, image);
  filter->SetInput(1, image);
  filter->SetInput(2, image);
  filter->SetInput(3, image);

  EXPECT_NO_THROW(filter->Update());
  EXPECT_EQ("2258f8cfbd7395bea0bc09ab3f69b058", MD5Hash(filter->GetOutput()));
  EXPECT_EQ(5, filter->GetOutput()->GetNumberOfComponentsPerPixel());

  layout[0] = 4;
  layout[1] = 1;
  EXPECT_NO_THROW(filter->SetLayout(layout));
  ITK_EXPECT_VECTOR_NEAR(layout, filter->GetLayout(), 0);

  EXPECT_NO_THROW(filter->UpdateLargestPossibleRegion());
  EXPECT_EQ("2258f8cfbd7395bea0bc09ab3f69b058", MD5Hash(filter->GetOutput()));
  EXPECT_EQ(5, filter->GetOutput()->GetNumberOfComponentsPerPixel());


  layout[0] = 1;
  layout[1] = 4;
  EXPECT_NO_THROW(filter->SetLayout(layout));
  ITK_EXPECT_VECTOR_NEAR(layout, filter->GetLayout(), 0);

  EXPECT_NO_THROW(filter->UpdateLargestPossibleRegion());
  EXPECT_EQ("2258f8cfbd7395bea0bc09ab3f69b058", MD5Hash(filter->GetOutput()));
  EXPECT_EQ(5, filter->GetOutput()->GetNumberOfComponentsPerPixel());


  layout[0] = 1;
  layout[1] = 10;
  EXPECT_NO_THROW(filter->SetLayout(layout));
  ITK_EXPECT_VECTOR_NEAR(layout, filter->GetLayout(), 0);

  EXPECT_NO_THROW(filter->UpdateLargestPossibleRegion());
  EXPECT_EQ("dd8552dd79d605d653872af827f2d1a5", MD5Hash(filter->GetOutput()));
  EXPECT_EQ(5, filter->GetOutput()->GetNumberOfComponentsPerPixel());


  auto image2 = ImageType::New();
  image2->SetNumberOfComponentsPerPixel(numberOfComponents - 1);
  image2->Allocate();

  filter->SetInput(1, image2);

  EXPECT_ANY_THROW(filter->Update());
}
