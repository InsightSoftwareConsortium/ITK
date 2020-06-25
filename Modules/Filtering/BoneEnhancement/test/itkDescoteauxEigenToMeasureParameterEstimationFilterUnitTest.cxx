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
#include "itkDescoteauxEigenToMeasureParameterEstimationFilter.h"
#include "itkImageMaskSpatialObject.h"
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace
{
template <typename T>
class itkDescoteauxEigenToMeasureParameterEstimationFilterUnitTest : public ::testing::Test
{
public:
  /* Useful typedefs */
  static const unsigned int DIMENSION = 3;
  using PixelType = T;
  using EigenValueArrayType = itk::FixedArray<PixelType, DIMENSION>;
  using EigenImageType = itk::Image<EigenValueArrayType, DIMENSION>;
  using MaskImageType = itk::Image<unsigned char, DIMENSION>;
  using FilterType = typename itk::DescoteauxEigenToMeasureParameterEstimationFilter<EigenImageType>;
  using FilterPointerType = typename FilterType::Pointer;
  using ParameterArrayType = typename FilterType::ParameterArrayType;
  using SpatialObjectType = itk::ImageMaskSpatialObject<DIMENSION>;

  itkDescoteauxEigenToMeasureParameterEstimationFilterUnitTest()
  {
    /* Instantiate filter */
    m_Filter = FilterType::New();

    /* Create EigenPixels */
    for (unsigned int i = 0; i < m_OneEigenPixel.Length; ++i)
    {
      m_OneEigenPixel[i] = 1;
      m_ZeroEigenPixel[i] = 0;
      m_LargeEigenPixel[i] = 100;
    }

    /* Create ImageRegion */
    typename EigenImageType::IndexType start;
    start[0] = 0;
    start[1] = 0;
    start[2] = 0;

    typename EigenImageType::SizeType size;
    size[0] = 10;
    size[1] = 10;
    size[2] = 10;

    m_Region.SetSize(size);
    m_Region.SetIndex(start);

    /* Create EigenImage */
    m_OnesEigenImage = EigenImageType::New();
    m_OnesEigenImage->SetRegions(m_Region);
    m_OnesEigenImage->Allocate();
    m_OnesEigenImage->FillBuffer(m_OneEigenPixel);

    m_ZerosEigenImage = EigenImageType::New();
    m_ZerosEigenImage->SetRegions(m_Region);
    m_ZerosEigenImage->Allocate();
    m_ZerosEigenImage->FillBuffer(m_ZeroEigenPixel);

    m_MaskingEigenImage = EigenImageType::New();
    m_MaskingEigenImage->SetRegions(m_Region);
    m_MaskingEigenImage->Allocate();
    m_MaskingEigenImage->FillBuffer(m_OneEigenPixel);

    /* Create MaskImage */
    typename EigenImageType::IndexType maskStart;
    maskStart[0] = 2;
    maskStart[1] = 2;
    maskStart[2] = 2;

    typename EigenImageType::SizeType maskSize;
    maskSize[0] = 12;
    maskSize[1] = 12;
    maskSize[2] = 12;

    typename EigenImageType::RegionType maskRegion;
    maskRegion.SetSize(maskSize);
    maskRegion.SetIndex(maskStart);

    m_MaskImage = MaskImageType::New();
    m_MaskImage->SetRegions(maskRegion);
    m_MaskImage->Allocate();
    m_MaskImage->FillBuffer(0);

    maskRegion.Crop(m_Region);
    itk::ImageRegionIteratorWithIndex<EigenImageType> input(m_MaskingEigenImage, maskRegion);
    itk::ImageRegionIteratorWithIndex<MaskImageType>  maskIt(m_MaskImage, maskRegion);

    input.GoToBegin();
    maskIt.GoToBegin();
    while (!input.IsAtEnd())
    {
      input.Set(m_LargeEigenPixel);
      maskIt.Set(1);
      ++input;
      ++maskIt;
    }

    /* Create Spatial Object */
    m_SpatialObject = SpatialObjectType::New();
    m_SpatialObject->SetImage(m_MaskImage);
  }
  ~itkDescoteauxEigenToMeasureParameterEstimationFilterUnitTest() override = default;

protected:
  void
  SetUp() override
  {}
  void
  TearDown() override
  {}

  FilterPointerType                   m_Filter;
  typename MaskImageType::Pointer     m_MaskImage;
  typename EigenImageType::Pointer    m_ZerosEigenImage;
  typename EigenImageType::Pointer    m_OnesEigenImage;
  typename EigenImageType::Pointer    m_MaskingEigenImage;
  EigenValueArrayType                 m_OneEigenPixel;
  EigenValueArrayType                 m_ZeroEigenPixel;
  EigenValueArrayType                 m_LargeEigenPixel;
  ParameterArrayType                  m_Parameters;
  typename EigenImageType::RegionType m_Region;
  typename SpatialObjectType::Pointer m_SpatialObject;
};
} // namespace

// Define the templates we would like to test
using TestingLabelTypes = ::testing::Types<double, float>;
TYPED_TEST_SUITE(itkDescoteauxEigenToMeasureParameterEstimationFilterUnitTest, TestingLabelTypes);

TYPED_TEST(itkDescoteauxEigenToMeasureParameterEstimationFilterUnitTest, InitialParameters)
{
  EXPECT_DOUBLE_EQ(0.5, this->m_Filter->GetFrobeniusNormWeight());

  this->m_Parameters = this->m_Filter->GetParameters();
  EXPECT_DOUBLE_EQ(0.5, this->m_Parameters[0]);
  EXPECT_DOUBLE_EQ(0.5, this->m_Parameters[1]);
  EXPECT_DOUBLE_EQ(1.0, this->m_Parameters[2]);
}

TYPED_TEST(itkDescoteauxEigenToMeasureParameterEstimationFilterUnitTest, TestZerosImage)
{
  this->m_Filter->SetInput(this->m_ZerosEigenImage);
  EXPECT_NO_THROW(this->m_Filter->Update());
  EXPECT_TRUE(this->m_Filter->GetOutput()->GetBufferedRegion() == this->m_Region);

  using EigenValueArrayType = itk::FixedArray<TypeParam, 3>;
  using ImageType = typename itk::Image<EigenValueArrayType, 3>;
  itk::ImageRegionIteratorWithIndex<ImageType> input(this->m_Filter->GetOutput(), this->m_Region);

  input.GoToBegin();
  while (!input.IsAtEnd())
  {
    ASSERT_TRUE(this->m_ZeroEigenPixel == input.Get());
    ++input;
  }

  this->m_Parameters = this->m_Filter->GetParameters();
  EXPECT_DOUBLE_EQ(0.5, this->m_Parameters[0]);
  EXPECT_DOUBLE_EQ(0.5, this->m_Parameters[1]);
  EXPECT_DOUBLE_EQ(0.0, this->m_Parameters[2]);
}

TYPED_TEST(itkDescoteauxEigenToMeasureParameterEstimationFilterUnitTest, TestOnesImage)
{
  this->m_Filter->SetInput(this->m_OnesEigenImage);
  EXPECT_NO_THROW(this->m_Filter->Update());
  EXPECT_TRUE(this->m_Filter->GetOutput()->GetBufferedRegion() == this->m_Region);

  using EigenValueArrayType = itk::FixedArray<TypeParam, 3>;
  using ImageType = typename itk::Image<EigenValueArrayType, 3>;
  itk::ImageRegionIteratorWithIndex<ImageType> input(this->m_Filter->GetOutput(), this->m_Region);

  input.GoToBegin();
  while (!input.IsAtEnd())
  {
    ASSERT_TRUE(this->m_OneEigenPixel == input.Get());
    ++input;
  }

  this->m_Parameters = this->m_Filter->GetParameters();
  EXPECT_DOUBLE_EQ(0.5, this->m_Parameters[0]);
  EXPECT_DOUBLE_EQ(0.5, this->m_Parameters[1]);
  EXPECT_NEAR(0.86602540378, this->m_Parameters[2], 1e-6); // sqrt(3) * 0.5
}

TYPED_TEST(itkDescoteauxEigenToMeasureParameterEstimationFilterUnitTest, TestOnesImageWithDifferentWeight)
{
  this->m_Filter->SetFrobeniusNormWeight(0.1);
  this->m_Filter->SetInput(this->m_OnesEigenImage);
  EXPECT_NO_THROW(this->m_Filter->Update());
  EXPECT_TRUE(this->m_Filter->GetOutput()->GetBufferedRegion() == this->m_Region);

  using EigenValueArrayType = itk::FixedArray<TypeParam, 3>;
  using ImageType = typename itk::Image<EigenValueArrayType, 3>;
  itk::ImageRegionIteratorWithIndex<ImageType> input(this->m_Filter->GetOutput(), this->m_Region);

  input.GoToBegin();
  while (!input.IsAtEnd())
  {
    ASSERT_TRUE(this->m_OneEigenPixel == input.Get());
    ++input;
  }

  this->m_Parameters = this->m_Filter->GetParameters();
  EXPECT_DOUBLE_EQ(0.5, this->m_Parameters[0]);
  EXPECT_DOUBLE_EQ(0.5, this->m_Parameters[1]);
  EXPECT_NEAR(0.17320508075, this->m_Parameters[2], 1e-6); // sqrt(3) * 0.1
}

TYPED_TEST(itkDescoteauxEigenToMeasureParameterEstimationFilterUnitTest, TestWithSpatialObject)
{
  this->m_Filter->SetInput(this->m_MaskingEigenImage);
  this->m_Filter->SetMask(this->m_SpatialObject);
  EXPECT_NO_THROW(this->m_Filter->Update());
  EXPECT_TRUE(this->m_Filter->GetOutput()->GetBufferedRegion() == this->m_Region);

  this->m_Parameters = this->m_Filter->GetParameters();
  EXPECT_DOUBLE_EQ(0.5, this->m_Parameters[0]);
  EXPECT_DOUBLE_EQ(0.5, this->m_Parameters[1]);
  EXPECT_NEAR(86.6025403784, this->m_Parameters[2], 1e-6); // sqrt(3) * 0.1
}
