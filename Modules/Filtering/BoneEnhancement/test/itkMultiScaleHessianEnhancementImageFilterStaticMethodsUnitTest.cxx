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

#include "itkMultiScaleHessianEnhancementImageFilter.h"
#include "gtest/gtest.h"
#include "itkMath.h"

TEST(itkMultiScaleHessianEnhancementImageFilterStaticMethodsUnitTest, GenerateSigmaArrayWithSizeZero)
{
  const unsigned int Dimension = 3;
  using PixelType = int;
  using ImageType = itk::Image<PixelType, Dimension>;
  using MultiScaleHessianEnhancementImageFilterType = itk::MultiScaleHessianEnhancementImageFilter<ImageType>;
  using ArrayType = MultiScaleHessianEnhancementImageFilterType::SigmaArrayType;

  ArrayType sigmaArray;
  EXPECT_ANY_THROW(sigmaArray = MultiScaleHessianEnhancementImageFilterType::GenerateLogarithmicSigmaArray(5, 5, 0));
  EXPECT_ANY_THROW(sigmaArray = MultiScaleHessianEnhancementImageFilterType::GenerateEquispacedSigmaArray(5, 5, 0));
}

TEST(itkMultiScaleHessianEnhancementImageFilterStaticMethodsUnitTest, GenerateSigmaArrayWithMinEqualMax)
{
  const unsigned int Dimension = 3;
  using PixelType = int;
  using ImageType = itk::Image<PixelType, Dimension>;
  using MultiScaleHessianEnhancementImageFilterType = itk::MultiScaleHessianEnhancementImageFilter<ImageType>;
  using ArrayType = MultiScaleHessianEnhancementImageFilterType::SigmaArrayType;

  /* Test that we get one when min equals max */
  ArrayType expectedOneSigmaArray;
  expectedOneSigmaArray.SetSize(1);
  expectedOneSigmaArray.SetElement(0, 1);

  /* Test the two cases of min equals max with a large number of values */
  ArrayType sigmaArray;
  sigmaArray = MultiScaleHessianEnhancementImageFilterType::GenerateLogarithmicSigmaArray(1, 1, 100);
  EXPECT_EQ(expectedOneSigmaArray, sigmaArray);
  sigmaArray = MultiScaleHessianEnhancementImageFilterType::GenerateEquispacedSigmaArray(1, 1, 100);
  EXPECT_EQ(expectedOneSigmaArray, sigmaArray);
}

TEST(itkMultiScaleHessianEnhancementImageFilterStaticMethodsUnitTest, GenerateEquispacedSigmaArray)
{
  const unsigned int Dimension = 3;
  using PixelType = int;
  using ImageType = itk::Image<PixelType, Dimension>;
  using MultiScaleHessianEnhancementImageFilterType = itk::MultiScaleHessianEnhancementImageFilter<ImageType>;
  using ArrayType = MultiScaleHessianEnhancementImageFilterType::SigmaArrayType;

  /* Expected array */
  ArrayType expectedArray;
  expectedArray.SetSize(5);
  expectedArray.SetElement(0, 1);
  expectedArray.SetElement(1, 2);
  expectedArray.SetElement(2, 3);
  expectedArray.SetElement(3, 4);
  expectedArray.SetElement(4, 5);

  /* Test */
  ArrayType sigmaArray;
  sigmaArray = MultiScaleHessianEnhancementImageFilterType::GenerateEquispacedSigmaArray(1, 5, 5);
  EXPECT_EQ(expectedArray.GetSize(), sigmaArray.GetSize());
  for (unsigned int i = 0; i < expectedArray.GetSize(); ++i)
  {
    EXPECT_DOUBLE_EQ(expectedArray.GetElement(i), sigmaArray.GetElement(i));
  }
}

TEST(itkMultiScaleHessianEnhancementImageFilterStaticMethodsUnitTest, GenerateEquispacedSigmaArrayWithFlip)
{
  const unsigned int Dimension = 3;
  using PixelType = int;
  using ImageType = itk::Image<PixelType, Dimension>;
  using MultiScaleHessianEnhancementImageFilterType = itk::MultiScaleHessianEnhancementImageFilter<ImageType>;
  using ArrayType = MultiScaleHessianEnhancementImageFilterType::SigmaArrayType;

  /* Expected array */
  ArrayType expectedArray;
  expectedArray.SetSize(5);
  expectedArray.SetElement(0, 1);
  expectedArray.SetElement(1, 2);
  expectedArray.SetElement(2, 3);
  expectedArray.SetElement(3, 4);
  expectedArray.SetElement(4, 5);

  /* Test */
  ArrayType sigmaArray;
  sigmaArray = MultiScaleHessianEnhancementImageFilterType::GenerateEquispacedSigmaArray(5, 1, 5);
  EXPECT_EQ(expectedArray.GetSize(), sigmaArray.GetSize());
  for (unsigned int i = 0; i < expectedArray.GetSize(); ++i)
  {
    EXPECT_DOUBLE_EQ(expectedArray.GetElement(i), sigmaArray.GetElement(i));
  }
}

TEST(itkMultiScaleHessianEnhancementImageFilterStaticMethodsUnitTest, GenerateLogarithmicSigmaArray)
{
  const unsigned int Dimension = 3;
  using PixelType = int;
  using ImageType = itk::Image<PixelType, Dimension>;
  using MultiScaleHessianEnhancementImageFilterType = itk::MultiScaleHessianEnhancementImageFilter<ImageType>;
  using ArrayType = MultiScaleHessianEnhancementImageFilterType::SigmaArrayType;

  /* Expected array */
  ArrayType expectedArray;
  expectedArray.SetSize(5);
  expectedArray.SetElement(0, 1);
  expectedArray.SetElement(1, 1.4953487812212205);
  expectedArray.SetElement(2, 2.23606797749979);
  expectedArray.SetElement(3, 3.3437015248821096);
  expectedArray.SetElement(4, 5);

  /* Test */
  ArrayType sigmaArray;
  sigmaArray = MultiScaleHessianEnhancementImageFilterType::GenerateLogarithmicSigmaArray(1, 5, 5);
  EXPECT_EQ(expectedArray.GetSize(), sigmaArray.GetSize());
  for (unsigned int i = 0; i < expectedArray.GetSize(); ++i)
  {
    EXPECT_DOUBLE_EQ(expectedArray.GetElement(i), sigmaArray.GetElement(i));
  }
}

TEST(itkMultiScaleHessianEnhancementImageFilterStaticMethodsUnitTest, GenerateLogarithmicSigmaArrayWithFlip)
{
  const unsigned int Dimension = 3;
  using PixelType = int;
  using ImageType = itk::Image<PixelType, Dimension>;
  using MultiScaleHessianEnhancementImageFilterType = itk::MultiScaleHessianEnhancementImageFilter<ImageType>;
  using ArrayType = MultiScaleHessianEnhancementImageFilterType::SigmaArrayType;

  /* Expected array */
  ArrayType expectedArray;
  expectedArray.SetSize(5);
  expectedArray.SetElement(0, 1);
  expectedArray.SetElement(1, 1.4953487812212205);
  expectedArray.SetElement(2, 2.23606797749979);
  expectedArray.SetElement(3, 3.3437015248821096);
  expectedArray.SetElement(4, 5);

  /* Test */
  ArrayType sigmaArray;
  sigmaArray = MultiScaleHessianEnhancementImageFilterType::GenerateLogarithmicSigmaArray(5, 1, 5);
  EXPECT_EQ(expectedArray.GetSize(), sigmaArray.GetSize());
  for (unsigned int i = 0; i < expectedArray.GetSize(); ++i)
  {
    EXPECT_DOUBLE_EQ(expectedArray.GetElement(i), sigmaArray.GetElement(i));
  }
}
