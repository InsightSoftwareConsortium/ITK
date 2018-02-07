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

#include "itkGTest.h"
#include "itkDescoteauxEigenToMeasureImageFilter.h"

namespace
{
template <typename T>
class itkDescoteauxEigenToMeasureFunctorUnitTest
  : public ::testing::Test
{
public:
  /* Useful typedefs */
  static const unsigned int DIMENSION = 3;
  typedef T                                             PixelType;
  typedef itk::FixedArray< float, DIMENSION >           EigenValueArrayType;
  typedef itk::Image< EigenValueArrayType, DIMENSION >  EigenImageType;
  typedef itk::Image< PixelType, DIMENSION >            ImageType;
  typedef typename itk::Functor::DescoteauxEigenToMeasureFunctor< EigenValueArrayType, PixelType >
                                                        FunctorType;
  typedef typename FunctorType::ParameterType           ParameterType;

  itkDescoteauxEigenToMeasureFunctorUnitTest() {
    m_Functor = FunctorType();
    m_EigenPixel = EigenValueArrayType();
  }
  ~itkDescoteauxEigenToMeasureFunctorUnitTest() override {}

protected:
  void SetUp() override {}
  void TearDown() override {}

  FunctorType         m_Functor;
  EigenValueArrayType m_EigenPixel;
  ParameterType       m_Parameters;
};
}

// Define the templates we would like to test
typedef ::testing::Types<double, float> TestingLabelTypes;
TYPED_TEST_CASE(itkDescoteauxEigenToMeasureFunctorUnitTest, TestingLabelTypes);

TYPED_TEST(itkDescoteauxEigenToMeasureFunctorUnitTest, InitialParameters) {
  EXPECT_DOUBLE_EQ(-1.0, this->m_Functor.GetEnhanceType());

  this->m_Parameters = this->m_Functor.GetParameters();
  EXPECT_DOUBLE_EQ(0.0, this->m_Parameters[0]);
  EXPECT_DOUBLE_EQ(0.0, this->m_Parameters[1]);
  EXPECT_DOUBLE_EQ(0.0, this->m_Parameters[2]);
}

TYPED_TEST(itkDescoteauxEigenToMeasureFunctorUnitTest, SetGetEnhanceType) {
  EXPECT_DOUBLE_EQ(-1.0, this->m_Functor.GetEnhanceType());
  this->m_Functor.SetEnhanceDarkObjects();
  EXPECT_DOUBLE_EQ(1.0, this->m_Functor.GetEnhanceType());
  this->m_Functor.SetEnhanceBrightObjects();
  EXPECT_DOUBLE_EQ(-1.0, this->m_Functor.GetEnhanceType());
}

TYPED_TEST(itkDescoteauxEigenToMeasureFunctorUnitTest, SetGetParameters) {
  this->m_Parameters = this->m_Functor.GetParameters();
  EXPECT_DOUBLE_EQ(0.0, this->m_Parameters[0]);
  EXPECT_DOUBLE_EQ(0.0, this->m_Parameters[1]);
  EXPECT_DOUBLE_EQ(0.0, this->m_Parameters[2]);

  this->m_Parameters[0] = 100;
  this->m_Parameters[1] = 200;
  this->m_Parameters[2] = 300;
  this->m_Functor.SetParameters(this->m_Parameters);
  this->m_Parameters = this->m_Functor.GetParameters();
  EXPECT_DOUBLE_EQ(100.0, this->m_Parameters[0]);
  EXPECT_DOUBLE_EQ(200.0, this->m_Parameters[1]);
  EXPECT_DOUBLE_EQ(300.0, this->m_Parameters[2]);
}

TYPED_TEST(itkDescoteauxEigenToMeasureFunctorUnitTest, BrightFirstParameterSet) {
  this->m_Parameters[0] = 0.5;
  this->m_Parameters[1] = 0.5;
  this->m_Parameters[2] = 0.25;
  this->m_Functor.SetParameters(this->m_Parameters);
  this->m_Functor.SetEnhanceBrightObjects();

  /* All zeros returns zero */
  this->m_EigenPixel[0] = 0;
  this->m_EigenPixel[1] = 0;
  this->m_EigenPixel[2] = 0;
  EXPECT_NEAR(0.0, this->m_Functor(this->m_EigenPixel), 1e-6);
}

TYPED_TEST(itkDescoteauxEigenToMeasureFunctorUnitTest, DarkFirstParameterSet) {
  this->m_Parameters[0] = 0.5;
  this->m_Parameters[1] = 0.5;
  this->m_Parameters[2] = 0.25;
  this->m_Functor.SetParameters(this->m_Parameters);
  this->m_Functor.SetEnhanceDarkObjects();

  /* All zeros returns zero */
  this->m_EigenPixel[0] = 0;
  this->m_EigenPixel[1] = 0;
  this->m_EigenPixel[2] = 0;
  EXPECT_NEAR(0.0, this->m_Functor(this->m_EigenPixel), 1e-6);
}

TYPED_TEST(itkDescoteauxEigenToMeasureFunctorUnitTest, BrightSecondParameterSet) {
  this->m_Parameters[0] = 0.5;
  this->m_Parameters[1] = 0.5;
  this->m_Parameters[2] = 0.25;
  this->m_Functor.SetParameters(this->m_Parameters);
  this->m_Functor.SetEnhanceBrightObjects();

  /* All zeros returns zero */
  this->m_EigenPixel[0] = 0;
  this->m_EigenPixel[1] = 0;
  this->m_EigenPixel[2] = 1;
  EXPECT_NEAR((TypeParam)0.0, this->m_Functor(this->m_EigenPixel), 1e-6);
}

TYPED_TEST(itkDescoteauxEigenToMeasureFunctorUnitTest, DarkSecondParameterSet) {
  this->m_Parameters[0] = 0.5;
  this->m_Parameters[1] = 0.5;
  this->m_Parameters[2] = 0.25;
  this->m_Functor.SetParameters(this->m_Parameters);
  this->m_Functor.SetEnhanceBrightObjects();

  /* All zeros returns zero */
  this->m_EigenPixel[0] = 0;
  this->m_EigenPixel[1] = 0;
  this->m_EigenPixel[2] = 1;
  EXPECT_NEAR((TypeParam)0.0, this->m_Functor(this->m_EigenPixel), 1e-6);
}

TYPED_TEST(itkDescoteauxEigenToMeasureFunctorUnitTest, BrightThirdParameterSet) {
  this->m_Parameters[0] = 0.5;
  this->m_Parameters[1] = 0.5;
  this->m_Parameters[2] = 0.25;
  this->m_Functor.SetParameters(this->m_Parameters);
  this->m_Functor.SetEnhanceBrightObjects();

  /* All zeros returns zero */
  this->m_EigenPixel[0] = 0;
  this->m_EigenPixel[1] = 0;
  this->m_EigenPixel[2] = -1;
  EXPECT_NEAR((TypeParam)0.999329187279, this->m_Functor(this->m_EigenPixel), 1e-6);
}

TYPED_TEST(itkDescoteauxEigenToMeasureFunctorUnitTest, DarkThirdParameterSet) {
  this->m_Parameters[0] = 0.5;
  this->m_Parameters[1] = 0.5;
  this->m_Parameters[2] = 0.25;
  this->m_Functor.SetParameters(this->m_Parameters);
  this->m_Functor.SetEnhanceDarkObjects();

  /* All zeros returns zero */
  this->m_EigenPixel[0] = 0;
  this->m_EigenPixel[1] = 0;
  this->m_EigenPixel[2] = -1;
  EXPECT_NEAR((TypeParam)0.0, this->m_Functor(this->m_EigenPixel), 1e-6);
}

TYPED_TEST(itkDescoteauxEigenToMeasureFunctorUnitTest, BrightFourthParameterSet) {
  this->m_Parameters[0] = 0.5;
  this->m_Parameters[1] = 0.5;
  this->m_Parameters[2] = 0.25;
  this->m_Functor.SetParameters(this->m_Parameters);
  this->m_Functor.SetEnhanceBrightObjects();

  /* All zeros returns zero */
  this->m_EigenPixel[0] = 0;
  this->m_EigenPixel[1] = 0;
  this->m_EigenPixel[2] = 1;
  EXPECT_NEAR((TypeParam)0.0, this->m_Functor(this->m_EigenPixel), 1e-6);
}

TYPED_TEST(itkDescoteauxEigenToMeasureFunctorUnitTest, DarkFourthParameterSet) {
  this->m_Parameters[0] = 0.5;
  this->m_Parameters[1] = 0.5;
  this->m_Parameters[2] = 0.25;
  this->m_Functor.SetParameters(this->m_Parameters);
  this->m_Functor.SetEnhanceDarkObjects();

  /* All zeros returns zero */
  this->m_EigenPixel[0] = 0;
  this->m_EigenPixel[1] = 0;
  this->m_EigenPixel[2] = 1;
  EXPECT_NEAR((TypeParam)0.999329187279, this->m_Functor(this->m_EigenPixel), 1e-6);
}

TYPED_TEST(itkDescoteauxEigenToMeasureFunctorUnitTest, BrightFifthParameterSet) {
  this->m_Parameters[0] = 0.5;
  this->m_Parameters[1] = 0.5;
  this->m_Parameters[2] = 0.25;
  this->m_Functor.SetParameters(this->m_Parameters);
  this->m_Functor.SetEnhanceBrightObjects();

  /* All zeros returns zero */
  this->m_EigenPixel[0] = 0.25;
  this->m_EigenPixel[1] = 1;
  this->m_EigenPixel[2] = -1;
  EXPECT_NEAR((TypeParam)0.0913983433747, this->m_Functor(this->m_EigenPixel), 1e-6);
}

TYPED_TEST(itkDescoteauxEigenToMeasureFunctorUnitTest, DarkFifthParameterSet) {
  this->m_Parameters[0] = 0.5;
  this->m_Parameters[1] = 0.5;
  this->m_Parameters[2] = 0.25;
  this->m_Functor.SetParameters(this->m_Parameters);
  this->m_Functor.SetEnhanceDarkObjects();

  /* All zeros returns zero */
  this->m_EigenPixel[0] = 0.25;
  this->m_EigenPixel[1] = 1;
  this->m_EigenPixel[2] = 1;
  EXPECT_NEAR((TypeParam)0.0913983433747, this->m_Functor(this->m_EigenPixel), 1e-6);
}

TYPED_TEST(itkDescoteauxEigenToMeasureFunctorUnitTest, BrightSixthParameterSet) {
  this->m_Parameters[0] = 0.25;
  this->m_Parameters[1] = 0.25;
  this->m_Parameters[2] = 0.5;
  this->m_Functor.SetParameters(this->m_Parameters);
  this->m_Functor.SetEnhanceBrightObjects();

  /* All zeros returns zero */
  this->m_EigenPixel[0] = 0.25;
  this->m_EigenPixel[1] = 1;
  this->m_EigenPixel[2] = -1;
  EXPECT_NEAR((TypeParam)0.000326373962098, this->m_Functor(this->m_EigenPixel), 1e-6);
}

TYPED_TEST(itkDescoteauxEigenToMeasureFunctorUnitTest, DarkSixthParameterSet) {
  this->m_Parameters[0] = 0.25;
  this->m_Parameters[1] = 0.25;
  this->m_Parameters[2] = 0.5;
  this->m_Functor.SetParameters(this->m_Parameters);
  this->m_Functor.SetEnhanceDarkObjects();

  /* All zeros returns zero */
  this->m_EigenPixel[0] = 0.25;
  this->m_EigenPixel[1] = 1;
  this->m_EigenPixel[2] = 1;
  EXPECT_NEAR((TypeParam)0.000326373962098, this->m_Functor(this->m_EigenPixel), 1e-6);
}
