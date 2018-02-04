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
#include "itkDescoteauxEigenToMeasureParameterEstimationFilter.h"

namespace
{
template <typename T>
class itkDescoteauxEigenToMeasureFunctorUnitTest
  : public ::testing::Test
{
public:
  /* Useful typedefs */
  static const unsigned int DIMENSION = 3;
  typedef T                                         PixelType;
  typedef  itk::FixedArray< PixelType, DIMENSION >  EigenValueArrayType;
  typedef typename itk::Functor::DescoteauxEigenParameterFunctor< EigenValueArrayType >
                                                    FunctorType;
  typedef typename FunctorType::ParameterType       ParameterType;

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
typedef ::testing::Types<char, int, float> TestingLabelTypes;
TYPED_TEST_CASE(itkDescoteauxEigenToMeasureFunctorUnitTest, TestingLabelTypes);

TYPED_TEST(itkDescoteauxEigenToMeasureFunctorUnitTest, InitialParameters) {
  EXPECT_DOUBLE_EQ(0.5, this->m_Functor.GetFrobeniusNormWeight());

  this->m_Parameters = this->m_Functor.GetComputedParameters();
  EXPECT_DOUBLE_EQ(0.5, this->m_Parameters[0]);
  EXPECT_DOUBLE_EQ(0.5, this->m_Parameters[1]);
  EXPECT_DOUBLE_EQ(0.0, this->m_Parameters[2]);
}

TYPED_TEST(itkDescoteauxEigenToMeasureFunctorUnitTest, SetGetFrobeniusNormWeight) {
  EXPECT_DOUBLE_EQ(0.5, this->m_Functor.GetFrobeniusNormWeight());
  this->m_Functor.SetFrobeniusNormWeight(0.1);
  EXPECT_DOUBLE_EQ(0.1, this->m_Functor.GetFrobeniusNormWeight());
}

TYPED_TEST(itkDescoteauxEigenToMeasureFunctorUnitTest, TestPixelOfZero) {
  this->m_EigenPixel[0] = 0;
  this->m_EigenPixel[1] = 0;
  this->m_EigenPixel[2] = 0;
  this->m_Functor.Initialize(1);
  this->m_Functor.ProcessPixel(this->m_EigenPixel, 0);

  this->m_Parameters = this->m_Functor.GetComputedParameters();
  EXPECT_DOUBLE_EQ(0.5, this->m_Parameters[0]);
  EXPECT_DOUBLE_EQ(0.5, this->m_Parameters[1]);
  EXPECT_DOUBLE_EQ(0.0, this->m_Parameters[2]);
}

TYPED_TEST(itkDescoteauxEigenToMeasureFunctorUnitTest, TestPixelOfOne) {
  this->m_EigenPixel[0] = 1;
  this->m_EigenPixel[1] = 1;
  this->m_EigenPixel[2] = 1;
  this->m_Functor.Initialize(1);
  this->m_Functor.ProcessPixel(this->m_EigenPixel, 0);

  this->m_Parameters = this->m_Functor.GetComputedParameters();
  EXPECT_DOUBLE_EQ(0.5, this->m_Parameters[0]);
  EXPECT_DOUBLE_EQ(0.5, this->m_Parameters[1]);
  EXPECT_NEAR(0.86602540378, this->m_Parameters[2], 1e-6); // sqrt(3) * 0.5
}

TYPED_TEST(itkDescoteauxEigenToMeasureFunctorUnitTest, TestPixelOfOneWithDifferentWeight) {
  this->m_EigenPixel[0] = 1;
  this->m_EigenPixel[1] = 1;
  this->m_EigenPixel[2] = 1;
  this->m_Functor.Initialize(1);
  this->m_Functor.ProcessPixel(this->m_EigenPixel, 0);
  this->m_Functor.SetFrobeniusNormWeight(0.1);

  this->m_Parameters = this->m_Functor.GetComputedParameters();
  EXPECT_DOUBLE_EQ(0.5, this->m_Parameters[0]);
  EXPECT_DOUBLE_EQ(0.5, this->m_Parameters[1]);
  EXPECT_NEAR(0.17320508075, this->m_Parameters[2], 1e-6); // sqrt(3) * 0.1
}

TYPED_TEST(itkDescoteauxEigenToMeasureFunctorUnitTest, TestMultiplePixels) {
  unsigned int num = 10;
  this->m_EigenPixel[0] = 0;
  this->m_EigenPixel[1] = 0;
  this->m_EigenPixel[2] = 0;
  this->m_Functor.Initialize(num);
  for (unsigned int i = 0; i < num; ++i) {
    for (unsigned int j = 0; j < this->m_EigenPixel.Length; ++j){
      this->m_EigenPixel[j] = this->m_EigenPixel[j] + 2;
    }

    this->m_Functor.ProcessPixel(this->m_EigenPixel, i);
  }

  this->m_Parameters = this->m_Functor.GetComputedParameters();
  EXPECT_DOUBLE_EQ(0.5, this->m_Parameters[0]);
  EXPECT_DOUBLE_EQ(0.5, this->m_Parameters[1]);
  EXPECT_NEAR(17.3205080757, this->m_Parameters[2], 1e-6); // sqrt(3*20^2) * 0.5
}
