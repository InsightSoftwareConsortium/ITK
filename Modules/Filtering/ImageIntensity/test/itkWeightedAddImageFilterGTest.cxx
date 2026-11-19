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
