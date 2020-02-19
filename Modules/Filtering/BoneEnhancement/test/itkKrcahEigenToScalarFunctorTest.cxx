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

#include "itkKrcahEigenToScalarFunctorImageFilter.h"
#include "itkTestingMacros.h"

int
itkKrcahEigenToScalarFunctorTest(int argc, char * argv[])
{
  /* type alias, instantiate functor */
  constexpr unsigned int Dimension = 3;
  using ImagePixelType = double;
  using ImageType = itk::Image<ImagePixelType, Dimension>;
  typename ImageType::Pointer image = ImageType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(image, Image, ImageBase);

  using EigenValueType = float;
  using EigenValueArrayType = itk::FixedArray<EigenValueType, Dimension>;
  using EigenValueImageType = itk::Image<EigenValueArrayType, Dimension>;
  typename EigenValueImageType::Pointer image2 = EigenValueImageType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(image2, Image, ImageBase);

  using FunctorType = itk::Functor::KrcahEigenToScalarFunctor<EigenValueArrayType, ImagePixelType>;
  FunctorType functor = FunctorType();

  /* Exercise basic set/get methods */
  functor.SetAlpha(0.5);
  ITK_TEST_SET_GET_VALUE(0.5, functor.GetAlpha());
  functor.SetBeta(0.5);
  ITK_TEST_SET_GET_VALUE(0.5, functor.GetBeta());
  functor.SetGamma(0.25);
  ITK_TEST_SET_GET_VALUE(0.25, functor.GetGamma());
  // Default should be -1
  ITK_TEST_SET_GET_VALUE(-1.0, functor.GetEnhanceType());
  functor.SetEnhanceDarkObjects();
  ITK_TEST_SET_GET_VALUE(1.0, functor.GetEnhanceType());
  functor.SetEnhanceBrightObjects();
  ITK_TEST_SET_GET_VALUE(-1.0, functor.GetEnhanceType());

  /* Test a few calculations */
  EigenValueArrayType mEigenValueArray;
  functor.SetAlpha(0.5);
  functor.SetBeta(0.5);
  functor.SetGamma(0.25);
  functor.SetEnhanceBrightObjects();

  /* All zeros returns zero */
  mEigenValueArray[0] = 0;
  mEigenValueArray[1] = 0;
  mEigenValueArray[2] = 0;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(functor(mEigenValueArray), 0.0, 6, 0.000001));

  /* lambda_2 zeros returns zero */
  mEigenValueArray[0] = 0;
  mEigenValueArray[1] = 0;
  mEigenValueArray[2] = 1;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(functor(mEigenValueArray), 0.0, 6, 0.000001));

  /* lambda_1 zero */
  functor.SetEnhanceBrightObjects();
  mEigenValueArray[0] = 0;
  mEigenValueArray[1] = 1;
  mEigenValueArray[2] = 1;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(functor(mEigenValueArray), -0.0183156368276, 6, 0.000001));

  /* lambda_1 zero; dark sheets */
  functor.SetEnhanceDarkObjects();
  mEigenValueArray[0] = 0;
  mEigenValueArray[1] = 1;
  mEigenValueArray[2] = 1;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(functor(mEigenValueArray), 0.0183156368276, 6, 0.000001));

  /* one, one, one */
  functor.SetEnhanceBrightObjects();
  mEigenValueArray[0] = 1;
  mEigenValueArray[1] = 1;
  mEigenValueArray[2] = 1;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(functor(mEigenValueArray), -0.000335462627903, 6, 0.000001));

  /* one, one, one; dark sheets */
  functor.SetEnhanceDarkObjects();
  mEigenValueArray[0] = 1;
  mEigenValueArray[1] = 1;
  mEigenValueArray[2] = 1;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(functor(mEigenValueArray), 0.000335462627903, 6, 0.000001));

  /* one, one, one negative */
  functor.SetEnhanceBrightObjects();
  mEigenValueArray[0] = -1;
  mEigenValueArray[1] = -1;
  mEigenValueArray[2] = -1;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(functor(mEigenValueArray), 0.000335462627903, 6, 0.000001));

  /* one, one, one negative; dark sheets */
  functor.SetEnhanceDarkObjects();
  mEigenValueArray[0] = -1;
  mEigenValueArray[1] = -1;
  mEigenValueArray[2] = -1;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(functor(mEigenValueArray), -0.000335462627903, 6, 0.000001));

  /* -1, -2, 3 */
  functor.SetEnhanceBrightObjects();
  mEigenValueArray[0] = -1;
  mEigenValueArray[1] = -2;
  mEigenValueArray[2] = 3;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(functor(mEigenValueArray), -0.15123975969, 6, 0.000001));

  /* -1, -2, 3; dark sheets */
  functor.SetEnhanceDarkObjects();
  mEigenValueArray[0] = -1;
  mEigenValueArray[1] = -2;
  mEigenValueArray[2] = 3;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(functor(mEigenValueArray), 0.15123975969, 6, 0.000001));

  /* Perfect Joint */
  functor.SetEnhanceBrightObjects();
  mEigenValueArray[0] = 0;
  mEigenValueArray[1] = 1000;
  mEigenValueArray[2] = 100000000;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(functor(mEigenValueArray), -1.0, 6, 0.000001));

  /* Perfect Joint; dark sheets */
  functor.SetEnhanceDarkObjects();
  mEigenValueArray[0] = 0;
  mEigenValueArray[1] = 1000;
  mEigenValueArray[2] = 100000000;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(functor(mEigenValueArray), 1.0, 6, 0.000001));

  /* Perfect Bone */
  functor.SetEnhanceBrightObjects();
  mEigenValueArray[0] = 0;
  mEigenValueArray[1] = -1000;
  mEigenValueArray[2] = -100000000;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(functor(mEigenValueArray), 1.0, 6, 0.000001));

  /* Perfect Bone; dark sheets */
  functor.SetEnhanceDarkObjects();
  mEigenValueArray[0] = 0;
  mEigenValueArray[1] = -1000;
  mEigenValueArray[2] = -100000000;
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(functor(mEigenValueArray), -1.0, 6, 0.000001));

  return EXIT_SUCCESS;
}
