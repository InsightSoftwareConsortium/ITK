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

#include "itkUnaryGeneratorImageFilter.h"
#include "itkBinaryGeneratorImageFilter.h"
#include "itkImage.h"
#include "itkImageRegionIterator.h"

#include "itkGTest.h"


namespace
{

template <unsigned int D, typename TPixelType = float>
struct Utilities
{
  static const unsigned int Dimension = D;

  using PixelType = TPixelType;
  using ImageType = itk::Image<PixelType, Dimension>;
  using IndexType = typename ImageType::IndexType;

  static typename ImageType::Pointer
  CreateImage()
  {
    typename ImageType::Pointer image = ImageType::New();

    typename ImageType::SizeType imageSize;
    imageSize.Fill(5);
    image->SetRegions(typename ImageType::RegionType(imageSize));
    image->Allocate();

    PixelType value = itk::NumericTraits<PixelType>::Zero;

    itk::ImageRegionIterator<ImageType> iter;

    while (!iter.IsAtEnd())
    {
      iter.Set(++value);
      ++iter;
    }

    return image;
  }

  static TPixelType
  MyUnaryFunction(const TPixelType & p)
  {
    return p + 10;
  }

  static TPixelType
  MyBinaryFunction1(TPixelType p1, TPixelType p2)
  {
    return p1 + 3 * p2;
  }

  static TPixelType
  MyBinaryFunction2(const TPixelType & p1, const TPixelType & p2)
  {
    return p1 * 2 + p2;
  }
};


} // namespace


TEST(UnaryGeneratorImageFilter, SetGetBasic)
{

  using Utils = Utilities<3>;

  auto image = Utils::CreateImage();


  using FilterType = itk::UnaryGeneratorImageFilter<Utils::ImageType, Utils::ImageType>;
  FilterType::Pointer filter = FilterType::New();
  filter->Print(std::cout);

  FilterType::ConstPointer constFilter = (const FilterType *)(filter.GetPointer());

  EXPECT_STREQ("UnaryGeneratorImageFilter", filter->GetNameOfClass());
  EXPECT_STREQ("InPlaceImageFilter", filter->Superclass::GetNameOfClass());
}

TEST(BinaryGeneratorImageFilter, SetGetBasic)
{

  using Utils = Utilities<3>;

  auto image = Utils::CreateImage();


  using FilterType = itk::BinaryGeneratorImageFilter<Utils::ImageType, Utils::ImageType, Utils::ImageType>;
  FilterType::Pointer filter = FilterType::New();
  filter->Print(std::cout);


  FilterType::ConstPointer constFilter = (const FilterType *)(filter.GetPointer());

  EXPECT_STREQ("BinaryGeneratorImageFilter", filter->GetNameOfClass());
  EXPECT_STREQ("InPlaceImageFilter", filter->Superclass::GetNameOfClass());


  EXPECT_NO_THROW(filter->SetConstant1(2.0));
  EXPECT_EQ(2.0, filter->GetConstant1());


  EXPECT_NO_THROW(filter->SetConstant2(4.0));
  EXPECT_EQ(4.0, filter->GetConstant2());
}


TEST(UnaryGeneratorImageFilter, SetFunctor)
{

  using Utils = Utilities<3, float>;

  auto image = Utils::CreateImage();
  image->FillBuffer(0.0);


  using FilterType = itk::UnaryGeneratorImageFilter<Utils::ImageType, Utils::ImageType>;
  using ValueFunctionType = FilterType::ValueFunctionType;
  using ConstRefFunctionType = FilterType::ConstRefFunctionType;

  auto filter = FilterType::New();
  filter->SetInput(image);

  Utils::ImageType::Pointer outputImage;

  Utils::IndexType idx;
  idx.Fill(0);

  // Test with C style function pointer
  filter->SetFunctor(static_cast<ValueFunctionType *>(std::cos));

  EXPECT_NO_THROW(filter->Update());

  outputImage = filter->GetOutput();
  ASSERT_TRUE(outputImage.IsNotNull());

  EXPECT_NEAR(1.0, outputImage->GetPixel(idx), 1e-8);


  filter->SetFunctor(Utils::MyUnaryFunction);
  EXPECT_NO_THROW(filter->Update());

  outputImage = filter->GetOutput();
  ASSERT_TRUE(outputImage.IsNotNull());

  EXPECT_NEAR(10.0, outputImage->GetPixel(idx), 1e-8);

  // test with std::functional
  std::function<float(float)> func1 = static_cast<ValueFunctionType *>(std::sin);

  filter->SetFunctor(func1);
  EXPECT_NO_THROW(filter->Update());
  ASSERT_TRUE(outputImage.IsNotNull());

  EXPECT_NEAR(0.0, outputImage->GetPixel(idx), 1e-8);


  std::function<float(const float &)> func2 = static_cast<ConstRefFunctionType *>(Utils::MyUnaryFunction);

  filter->SetFunctor(func2);
  EXPECT_NO_THROW(filter->Update());
  ASSERT_TRUE(outputImage.IsNotNull());

  EXPECT_NEAR(10.0, outputImage->GetPixel(idx), 1e-8);


  // test with C++ lambda function
  filter->SetFunctor([](const float & v) { return v + 2.0; });
  EXPECT_NO_THROW(filter->Update());
  ASSERT_TRUE(outputImage.IsNotNull());

  EXPECT_NEAR(2.0, outputImage->GetPixel(idx), 1e-8);
}


TEST(BinaryGeneratorImageFilter, SetFunctor)
{

  using Utils = Utilities<3, float>;

  auto image1 = Utils::CreateImage();
  image1->FillBuffer(1.0);

  auto image2 = Utils::CreateImage();
  image2->FillBuffer(2.0);


  using FilterType = itk::BinaryGeneratorImageFilter<Utils::ImageType, Utils::ImageType, Utils::ImageType>;

  auto filter = FilterType::New();
  filter->SetInput1(image1);
  filter->SetInput2(image2);

  Utils::ImageType::Pointer outputImage;

  Utils::IndexType idx;
  idx.Fill(0);

  // Test with C style function pointer
  filter->SetFunctor(Utils::MyBinaryFunction1);
  EXPECT_NO_THROW(filter->Update());

  outputImage = filter->GetOutput();
  ASSERT_TRUE(outputImage.IsNotNull());

  EXPECT_NEAR(7.0, outputImage->GetPixel(idx), 1e-8);


  filter->SetFunctor(Utils::MyBinaryFunction2);
  EXPECT_NO_THROW(filter->Update());

  outputImage = filter->GetOutput();
  ASSERT_TRUE(outputImage.IsNotNull());

  EXPECT_NEAR(4.0, outputImage->GetPixel(idx), 1e-8);

  // test with std::functional
  std::function<float(float, float)> func1 = Utils::MyBinaryFunction1;

  filter->SetFunctor(func1);
  EXPECT_NO_THROW(filter->Update());
  ASSERT_TRUE(outputImage.IsNotNull());

  EXPECT_NEAR(7.0, outputImage->GetPixel(idx), 1e-8);


  std::function<float(const float &, const float &)> func2 = Utils::MyBinaryFunction2;

  filter->SetFunctor(func2);
  EXPECT_NO_THROW(filter->Update());
  ASSERT_TRUE(outputImage.IsNotNull());

  EXPECT_NEAR(4.0, outputImage->GetPixel(idx), 1e-8);


  // test with C++ lambda function
  filter->SetFunctor([](const float & v1, const float & v2) { return v1 * v2; });
  EXPECT_NO_THROW(filter->Update());
  ASSERT_TRUE(outputImage.IsNotNull());

  EXPECT_NEAR(2.0, outputImage->GetPixel(idx), 1e-8);
}
