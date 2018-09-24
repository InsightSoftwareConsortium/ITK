/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#include "itkDescoteauxEigenToScalarFunctorImageFilter.h"
#include "itkTestingMacros.h"

int itkDescoteauxEigenToScalarFunctorTest( int argc, char * argv[] )
{
  /* typedefs, instantiate functor */
  const unsigned int                              Dimension = 3;
  typedef double                                  ImagePixelType;
  typedef itk::Image< ImagePixelType, Dimension > ImageType;

  typedef float                                         EigenValueType;
  typedef itk::FixedArray< EigenValueType, Dimension >  EigenValueArrayType;
  typedef itk::Image< EigenValueArrayType, Dimension >  EigenValueImageType;

  typedef itk::Functor::DescoteauxEigenToScalarFunctor< EigenValueArrayType, ImagePixelType> FunctorType;
  FunctorType functor = FunctorType();

  /* Exercise basic set/get methods */
  functor.SetAlpha(0.25);
  TEST_SET_GET_VALUE(0.25, functor.GetAlpha());
  functor.SetBeta(0.25);
  TEST_SET_GET_VALUE(0.25, functor.GetBeta());
  functor.SetC(0.25);
  TEST_SET_GET_VALUE(0.25, functor.GetC());
  // Default should be -1
  TEST_SET_GET_VALUE(-1.0, functor.GetEnhanceType());
  functor.SetEnhanceDarkObjects();
  TEST_SET_GET_VALUE(1.0, functor.GetEnhanceType());
  functor.SetEnhanceBrightObjects();
  TEST_SET_GET_VALUE(-1.0, functor.GetEnhanceType());

  /* Test a few calculations */
  EigenValueArrayType mEigenValueArray;
  functor.SetAlpha(0.5);
  functor.SetBeta(0.5);
  functor.SetC(0.25);
  functor.SetEnhanceBrightObjects();

  /* All zeros returns zero */
  mEigenValueArray[0] = 0;
  mEigenValueArray[1] = 0;
  mEigenValueArray[2] = 0;
  TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual( functor(mEigenValueArray), 0.0, 6, 0.000001));

  /* Return zero on positive l3 with bright sheets */
  mEigenValueArray[0] = 0;
  mEigenValueArray[1] = 0;
  mEigenValueArray[2] = 1;
  functor.SetEnhanceBrightObjects();
  TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual( functor(mEigenValueArray), 0.0, 6, 0.000001));

  /* Return zero on positive l3 with dark sheets */
  mEigenValueArray[0] = 0;
  mEigenValueArray[1] = 0;
  mEigenValueArray[2] = -1;
  functor.SetEnhanceDarkObjects();
  TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual( functor(mEigenValueArray), 0.0, 6, 0.000001));

  /* 0, 0, -1; bright sheets */
  mEigenValueArray[0] = 0;
  mEigenValueArray[1] = 0;
  mEigenValueArray[2] = -1;
  functor.SetEnhanceBrightObjects();
  TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual( functor(mEigenValueArray), 0.999329187279, 6, 0.000001));

  /* 0, 0, 1; dark sheets */
  mEigenValueArray[0] = 0;
  mEigenValueArray[1] = 0;
  mEigenValueArray[2] = 1;
  functor.SetEnhanceDarkObjects();
  TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual( functor(mEigenValueArray), 0.999329187279, 6, 0.000001));

  /* 1, 1, -1; bright sheets */
  mEigenValueArray[0] = 1;
  mEigenValueArray[1] = 1;
  mEigenValueArray[2] = -1;
  functor.SetEnhanceBrightObjects();
  TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual( functor(mEigenValueArray), 0.0, 6, 0.000001));

  /* 1, 1, 1; dark sheets */
  mEigenValueArray[0] = 1;
  mEigenValueArray[1] = 1;
  mEigenValueArray[2] = 1;
  functor.SetEnhanceDarkObjects();
  TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual( functor(mEigenValueArray), 0.0, 6, 0.000001));

  /* 0.25, 1, -1; bright sheets */
  mEigenValueArray[0] = 0.25;
  mEigenValueArray[1] = 1;
  mEigenValueArray[2] = -1;
  functor.SetEnhanceBrightObjects();
  TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual( functor(mEigenValueArray), 0.0913983433747, 6, 0.000001));

  /* 0.25, 1, 1; dark sheets */
  mEigenValueArray[0] = 0.25;
  mEigenValueArray[1] = 1;
  mEigenValueArray[2] = 1;
  functor.SetEnhanceDarkObjects();
  TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual( functor(mEigenValueArray), 0.0913983433747, 6, 0.000001));
  
  return EXIT_SUCCESS;
}
