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

#include "itkWeightedAddImageFilter.h"
#include "itkVectorImage.h"
#include "itkVector.h"
#include "itkRGBPixel.h"
#include "itkNumericTraitsVectorPixel.h"
#include "itkNumericTraitsRGBPixel.h"
#include "itkGTest.h"


TEST(WeightedAddImageFilter, DefaultFunctorWeighsSecondOperandByOne)
{
  using FunctorType = itk::Functor::WeightedAdd2<short, short, short>;

  const FunctorType defaultFunctor;
  EXPECT_EQ(defaultFunctor.GetAlpha(), 0.0);
  EXPECT_EQ(defaultFunctor(3, 5), 5);

  FunctorType zeroAlphaFunctor;
  zeroAlphaFunctor.SetAlpha(0.0);
  EXPECT_EQ(defaultFunctor, zeroAlphaFunctor);
  EXPECT_EQ(defaultFunctor(3, 5), zeroAlphaFunctor(3, 5));
}


TEST(WeightedAddImageFilter, DefaultAlphaWeighsSecondInputByOne)
{
  using ImageType = itk::Image<short, 2>;
  using FilterType = itk::WeightedAddImageFilter<ImageType, ImageType, ImageType>;

  auto filter = FilterType::New();
  EXPECT_EQ(filter->GetAlpha(), 0.0);

  auto image = ImageType::New();
  image->SetRegions(itk::MakeSize(2, 2));
  image->Allocate();
  image->FillBuffer(7);

  filter->SetInput1(image);
  filter->SetConstant2(5);
  filter->Update();
  EXPECT_EQ(filter->GetOutput()->GetPixel({ { 0, 0 } }), 5);

  filter->SetConstant1(3);
  filter->SetInput2(image);
  filter->Update();
  EXPECT_EQ(filter->GetOutput()->GetPixel({ { 0, 0 } }), 7);
}


TEST(WeightedAddImageFilter, ScalarImage)
{
  using ImageType = itk::Image<float, 2>;
  using FilterType = itk::WeightedAddImageFilter<ImageType, ImageType, ImageType>;

  const ImageType::RegionType region{ itk::Size<2>::Filled(2) };

  auto image1 = ImageType::New();
  image1->SetRegions(region);
  image1->Allocate();
  image1->FillBuffer(10.0f);

  auto image2 = ImageType::New();
  image2->SetRegions(region);
  image2->Allocate();
  image2->FillBuffer(20.0f);

  auto filter = FilterType::New();
  filter->SetInput1(image1);
  filter->SetInput2(image2);
  filter->SetAlpha(0.25);
  ASSERT_NO_THROW(filter->Update());

  constexpr float expected = 0.25f * 10.0f + 0.75f * 20.0f;
  EXPECT_FLOAT_EQ(filter->GetOutput()->GetPixel({ { 0, 0 } }), expected);
}

// Alpha is a plain scalar even when the pixel type is a VariableLengthVector,
// via NumericTraits<TInput1>::ScalarRealType rather than ::RealType.
TEST(WeightedAddImageFilter, VectorImage)
{
  using ImageType = itk::VectorImage<float, 2>;
  using FilterType = itk::WeightedAddImageFilter<ImageType, ImageType, ImageType>;

  const ImageType::RegionType region{ itk::Size<2>::Filled(2) };
  constexpr unsigned int      numberOfComponents = 3;

  itk::VariableLengthVector<float> value1(numberOfComponents);
  itk::VariableLengthVector<float> value2(numberOfComponents);
  itk::VariableLengthVector<float> expected(numberOfComponents);
  for (unsigned int i = 0; i < numberOfComponents; ++i)
  {
    value1[i] = static_cast<float>(i + 1);        // 1, 2, 3
    value2[i] = static_cast<float>(10 * (i + 1)); // 10, 20, 30
    expected[i] = 0.25f * value1[i] + 0.75f * value2[i];
  }

  auto image1 = ImageType::New();
  image1->SetRegions(region);
  image1->SetNumberOfComponentsPerPixel(numberOfComponents);
  image1->Allocate();
  image1->FillBuffer(value1);

  auto image2 = ImageType::New();
  image2->SetRegions(region);
  image2->SetNumberOfComponentsPerPixel(numberOfComponents);
  image2->Allocate();
  image2->FillBuffer(value2);

  auto filter = FilterType::New();
  filter->SetInput1(image1);
  filter->SetInput2(image2);
  filter->SetAlpha(0.25);
  ASSERT_NO_THROW(filter->Update());

  EXPECT_EQ(filter->GetOutput()->GetPixel({ { 0, 0 } }), expected);
}

namespace
{
// Fixed-size pixels with an integral component type must blend in real
// arithmetic; a fractional alpha must not be truncated to the component type.
template <typename TPixel>
void
ExpectIntegralFixedSizePixelBlend()
{
  using ImageType = itk::Image<TPixel, 2>;
  using FilterType = itk::WeightedAddImageFilter<ImageType, ImageType, ImageType>;

  const typename ImageType::RegionType region{ itk::Size<2>::Filled(2) };

  TPixel value1;
  value1.Fill(10);
  TPixel value2;
  value2.Fill(20);

  auto image1 = ImageType::New();
  image1->SetRegions(region);
  image1->Allocate();
  image1->FillBuffer(value1);

  auto image2 = ImageType::New();
  image2->SetRegions(region);
  image2->Allocate();
  image2->FillBuffer(value2);

  auto filter = FilterType::New();
  filter->SetInput1(image1);
  filter->SetInput2(image2);
  filter->SetAlpha(0.25);
  ASSERT_NO_THROW(filter->Update());

  // 0.25 * 10 + 0.75 * 20 = 17.5, truncated by the output cast.
  TPixel expected;
  expected.Fill(17);
  EXPECT_EQ(filter->GetOutput()->GetPixel({ { 0, 0 } }), expected);
}
} // namespace

TEST(WeightedAddImageFilter, IntegralVectorImage) { ExpectIntegralFixedSizePixelBlend<itk::Vector<int, 3>>(); }

TEST(WeightedAddImageFilter, UnsignedCharRGBImage)
{
  ExpectIntegralFixedSizePixelBlend<itk::RGBPixel<unsigned char>>();
}
