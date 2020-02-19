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
#include "itkArray.h"
#include "itkTestingMacros.h"
#include "itkMath.h"

int
itkMultiScaleHessianEnhancementImageFilterStaticMethodsTest(int, char *[])
{
  constexpr unsigned int Dimension = 2;
  using PixelType = int;
  using ImageType = itk::Image<PixelType, Dimension>;
  using MultiScaleHessianEnhancementImageFilterType = itk::MultiScaleHessianEnhancementImageFilter<ImageType>;
  using ArrayType = MultiScaleHessianEnhancementImageFilterType::SigmaArrayType;

  /* Test the two cases of step size zero */
  ArrayType sigmaArray;

  ITK_TRY_EXPECT_EXCEPTION(sigmaArray =
                             MultiScaleHessianEnhancementImageFilterType::GenerateLogarithmicSigmaArray(5, 5, 0));

  ITK_TRY_EXPECT_EXCEPTION(sigmaArray =
                             MultiScaleHessianEnhancementImageFilterType::GenerateEquispacedSigmaArray(5, 5, 0));

  /* Test that we get one when min equals max */
  ArrayType expectedOneSigmaArray;
  expectedOneSigmaArray.SetSize(1);
  expectedOneSigmaArray.SetElement(0, 1);

  /* Test the two cases of min equals max with a large number of values */
  sigmaArray = MultiScaleHessianEnhancementImageFilterType::GenerateLogarithmicSigmaArray(1, 1, 100);
  ITK_TEST_EXPECT_EQUAL(expectedOneSigmaArray, sigmaArray);
  sigmaArray = MultiScaleHessianEnhancementImageFilterType::GenerateEquispacedSigmaArray(1, 1, 100);
  ITK_TEST_EXPECT_EQUAL(expectedOneSigmaArray, sigmaArray);

  /* Test the Logarithmic method */
  ArrayType expectedLogarithmicArray;
  expectedLogarithmicArray.SetSize(5);
  expectedLogarithmicArray.SetElement(0, 1);
  expectedLogarithmicArray.SetElement(1, 1.4953487812212205);
  expectedLogarithmicArray.SetElement(2, 2.23606797749979);
  expectedLogarithmicArray.SetElement(3, 3.3437015248821096);
  expectedLogarithmicArray.SetElement(4, 5);

  ArrayType logarithmicArray = MultiScaleHessianEnhancementImageFilterType::GenerateLogarithmicSigmaArray(1, 5, 5);
  /* Here we need to do some floating point comparisons */
  ITK_TEST_EXPECT_EQUAL(expectedLogarithmicArray.GetSize(), logarithmicArray.GetSize())
  for (ArrayType::SizeValueType i = 0; i < expectedLogarithmicArray.GetSize(); ++i)
  {
    ITK_TEST_EXPECT_TRUE(
      itk::Math::FloatAlmostEqual(expectedLogarithmicArray.GetElement(i), logarithmicArray.GetElement(i), 6, 0.000001));
  }

  logarithmicArray = MultiScaleHessianEnhancementImageFilterType::GenerateLogarithmicSigmaArray(5, 1, 5);
  /* Should be the same if we flip min and max */
  ITK_TEST_EXPECT_EQUAL(expectedLogarithmicArray.GetSize(), logarithmicArray.GetSize())
  for (ArrayType::SizeValueType i = 0; i < expectedLogarithmicArray.GetSize(); ++i)
  {
    ITK_TEST_EXPECT_TRUE(
      itk::Math::FloatAlmostEqual(expectedLogarithmicArray.GetElement(i), logarithmicArray.GetElement(i), 6, 0.000001));
  }

  /* Test the Equidistance method */
  ArrayType expectedEquidistanceArray;
  expectedEquidistanceArray.SetSize(5);
  expectedEquidistanceArray.SetElement(0, 1.0);
  expectedEquidistanceArray.SetElement(1, 2.0);
  expectedEquidistanceArray.SetElement(2, 3.0);
  expectedEquidistanceArray.SetElement(3, 4.0);
  expectedEquidistanceArray.SetElement(4, 5.0);

  ArrayType equidistanceArray = MultiScaleHessianEnhancementImageFilterType::GenerateEquispacedSigmaArray(1, 5, 5);
  /* Here we need to do some floating point comparisons. These are exact, but we should still do a floating point
   * comparison to be sure */
  ITK_TEST_EXPECT_EQUAL(expectedEquidistanceArray.GetSize(), equidistanceArray.GetSize())
  for (ArrayType::SizeValueType i = 0; i < expectedEquidistanceArray.GetSize(); ++i)
  {
    ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(
      expectedEquidistanceArray.GetElement(i), equidistanceArray.GetElement(i), 6, 0.000001));
  }

  equidistanceArray = MultiScaleHessianEnhancementImageFilterType::GenerateEquispacedSigmaArray(5, 1, 5);
  /* Should be the same if we flip min and max */
  ITK_TEST_EXPECT_EQUAL(expectedEquidistanceArray.GetSize(), equidistanceArray.GetSize())
  for (ArrayType::SizeValueType i = 0; i < expectedEquidistanceArray.GetSize(); ++i)
  {
    ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(
      expectedEquidistanceArray.GetElement(i), equidistanceArray.GetElement(i), 6, 0.000001));
  }

  return EXIT_SUCCESS;
}
