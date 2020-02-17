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
#include "itkImageMaskSpatialObject.h"
#include "itkImage.h"
#include "itkImageRegionIteratorWithIndex.h"

namespace
{
template <typename T>
class itkDescoteauxEigenToMeasureImageFilterUnitTest : public ::testing::Test
{
public:
  /* Useful typedefs */
  static const unsigned int DIMENSION = 3;
  using PixelType = T;
  using OutputPixelType = itk::Image<PixelType, DIMENSION>;
  using EigenPixelType = float;
  using EigenValueArrayType = itk::FixedArray<EigenPixelType, DIMENSION>;
  using EigenImageType = itk::Image<EigenValueArrayType, DIMENSION>;
  using MaskImageType = itk::Image<unsigned char, DIMENSION>;
  using FilterType = typename itk::DescoteauxEigenToMeasureImageFilter<EigenImageType, OutputPixelType>;
  using FilterPointerType = typename FilterType::Pointer;
  using ParameterArrayType = typename FilterType::ParameterArrayType;
  using SpatialObjectType = itk::ImageMaskSpatialObject<DIMENSION>;

  itkDescoteauxEigenToMeasureImageFilterUnitTest()
  {
    /* Instantiate filter */
    m_Filter = FilterType::New();

    /* Set parameter size */
    m_Parameters.SetSize(3);

    /* Create EigenPixels */
    for (unsigned int i = 0; i < m_OneEigenPixel.Length; ++i)
    {
      m_OneEigenPixel[i] = 1;
      m_ZeroEigenPixel[i] = 0;
    }
    m_NonZeroEigenPixel[0] = 0.25;
    m_NonZeroEigenPixel[1] = 1;
    m_NonZeroEigenPixel[2] = -1;
    m_NonZeroDarkEigenPixel[0] = 0.25;
    m_NonZeroDarkEigenPixel[1] = 1;
    m_NonZeroDarkEigenPixel[2] = 1;

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

    m_NonZeroEigenImage = EigenImageType::New();
    m_NonZeroEigenImage->SetRegions(m_Region);
    m_NonZeroEigenImage->Allocate();
    m_NonZeroEigenImage->FillBuffer(m_NonZeroEigenPixel);

    m_NonZeroDarkEigenImage = EigenImageType::New();
    m_NonZeroDarkEigenImage->SetRegions(m_Region);
    m_NonZeroDarkEigenImage->Allocate();
    m_NonZeroDarkEigenImage->FillBuffer(m_NonZeroDarkEigenPixel);

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

    m_MaskRegion.SetSize(maskSize);
    m_MaskRegion.SetIndex(maskStart);

    m_MaskImage = MaskImageType::New();
    m_MaskImage->SetRegions(m_MaskRegion);
    m_MaskImage->Allocate();
    m_MaskImage->FillBuffer(0);

    m_MaskRegion.Crop(m_Region);
    itk::ImageRegionIteratorWithIndex<MaskImageType> maskIt(m_MaskImage, m_MaskRegion);

    maskIt.GoToBegin();
    while (!maskIt.IsAtEnd())
    {
      maskIt.Set(1);
      ++maskIt;
    }

    m_SpatialObject = SpatialObjectType::New();
    m_SpatialObject->SetImage(m_MaskImage);
  }
  ~itkDescoteauxEigenToMeasureImageFilterUnitTest() override = default;

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
  typename EigenImageType::Pointer    m_NonZeroEigenImage;
  typename EigenImageType::Pointer    m_NonZeroDarkEigenImage;
  typename EigenImageType::Pointer    m_MaskingEigenImage;
  EigenValueArrayType                 m_OneEigenPixel;
  EigenValueArrayType                 m_ZeroEigenPixel;
  EigenValueArrayType                 m_NonZeroEigenPixel;
  EigenValueArrayType                 m_NonZeroDarkEigenPixel;
  ParameterArrayType                  m_Parameters;
  typename EigenImageType::RegionType m_Region;
  typename EigenImageType::RegionType m_MaskRegion;
  typename SpatialObjectType::Pointer m_SpatialObject;
};
} // namespace

// Define the templates we would like to test
using TestingLabelTypes = ::testing::Types<double, float>;
TYPED_TEST_CASE(itkDescoteauxEigenToMeasureImageFilterUnitTest, TestingLabelTypes);

TYPED_TEST(itkDescoteauxEigenToMeasureImageFilterUnitTest, InitialParameters)
{
  /* Default enhance bright structures */
  EXPECT_DOUBLE_EQ(-1.0, this->m_Filter->GetEnhanceType());

  EXPECT_EQ(2, static_cast<int>(this->m_Filter->GetEigenValueOrder()));
}

TYPED_TEST(itkDescoteauxEigenToMeasureImageFilterUnitTest, TestZerosImage)
{
  this->m_Parameters[0] = 0.5;
  this->m_Parameters[1] = 0.5;
  this->m_Parameters[2] = 1;
  this->m_Filter->SetParameters(this->m_Parameters);
  this->m_Filter->SetInput(this->m_ZerosEigenImage);
  this->m_Filter->Update();
  EXPECT_NO_THROW(this->m_Filter->Update());
  EXPECT_TRUE(this->m_Filter->GetOutput()->GetBufferedRegion() == this->m_Region);

  using ImageType = typename itk::Image<TypeParam, 3>;
  itk::ImageRegionIteratorWithIndex<ImageType> input(this->m_Filter->GetOutput(), this->m_Region);

  input.GoToBegin();
  while (!input.IsAtEnd())
  {
    ASSERT_EQ(0.0, input.Get());
    ++input;
  }
}

TYPED_TEST(itkDescoteauxEigenToMeasureImageFilterUnitTest, TestRealEigenPixelBrightSheet)
{
  this->m_Parameters[0] = 0.5;
  this->m_Parameters[1] = 0.5;
  this->m_Parameters[2] = 0.25;
  this->m_Filter->SetParameters(this->m_Parameters);
  this->m_Filter->SetInput(this->m_NonZeroEigenImage);
  EXPECT_NO_THROW(this->m_Filter->Update());
  EXPECT_TRUE(this->m_Filter->GetOutput()->GetBufferedRegion() == this->m_Region);

  using ImageType = typename itk::Image<TypeParam, 3>;
  itk::ImageRegionIteratorWithIndex<ImageType> input(this->m_Filter->GetOutput(), this->m_Region);

  input.GoToBegin();
  while (!input.IsAtEnd())
  {
    ASSERT_NEAR((TypeParam)0.0913983433747, input.Get(), 1e-6);
    ++input;
  }
}

TYPED_TEST(itkDescoteauxEigenToMeasureImageFilterUnitTest, TestRealEigenPixelDarkSheet)
{
  this->m_Parameters[0] = 0.5;
  this->m_Parameters[1] = 0.5;
  this->m_Parameters[2] = 0.25;
  this->m_Filter->SetParameters(this->m_Parameters);
  this->m_Filter->SetInput(this->m_NonZeroEigenImage);
  this->m_Filter->SetEnhanceDarkObjects();
  EXPECT_NO_THROW(this->m_Filter->Update());
  EXPECT_TRUE(this->m_Filter->GetOutput()->GetBufferedRegion() == this->m_Region);

  using ImageType = typename itk::Image<TypeParam, 3>;
  itk::ImageRegionIteratorWithIndex<ImageType> input(this->m_Filter->GetOutput(), this->m_Region);

  input.GoToBegin();
  while (!input.IsAtEnd())
  {
    ASSERT_NEAR((TypeParam)0.0, input.Get(), 1e-6);
    ++input;
  }
}

TYPED_TEST(itkDescoteauxEigenToMeasureImageFilterUnitTest, TestDarkRealEigenPixelBrightSheet)
{
  this->m_Parameters[0] = 0.5;
  this->m_Parameters[1] = 0.5;
  this->m_Parameters[2] = 0.25;
  this->m_Filter->SetParameters(this->m_Parameters);
  this->m_Filter->SetInput(this->m_NonZeroDarkEigenImage);
  EXPECT_NO_THROW(this->m_Filter->Update());
  EXPECT_TRUE(this->m_Filter->GetOutput()->GetBufferedRegion() == this->m_Region);

  using ImageType = typename itk::Image<TypeParam, 3>;
  itk::ImageRegionIteratorWithIndex<ImageType> input(this->m_Filter->GetOutput(), this->m_Region);

  input.GoToBegin();
  while (!input.IsAtEnd())
  {
    ASSERT_NEAR((TypeParam)0.0, input.Get(), 1e-6);
    ++input;
  }
}

TYPED_TEST(itkDescoteauxEigenToMeasureImageFilterUnitTest, TestDarkRealEigenPixelDarkSheet)
{
  this->m_Parameters[0] = 0.25;
  this->m_Parameters[1] = 0.25;
  this->m_Parameters[2] = 0.5;
  this->m_Filter->SetParameters(this->m_Parameters);
  this->m_Filter->SetInput(this->m_NonZeroDarkEigenImage);
  this->m_Filter->SetEnhanceDarkObjects();
  EXPECT_NO_THROW(this->m_Filter->Update());
  EXPECT_TRUE(this->m_Filter->GetOutput()->GetBufferedRegion() == this->m_Region);

  using ImageType = typename itk::Image<TypeParam, 3>;
  itk::ImageRegionIteratorWithIndex<ImageType> input(this->m_Filter->GetOutput(), this->m_Region);

  input.GoToBegin();
  while (!input.IsAtEnd())
  {
    ASSERT_NEAR((TypeParam)0.000326373962098, input.Get(), 1e-6);
    ++input;
  }
}

TYPED_TEST(itkDescoteauxEigenToMeasureImageFilterUnitTest, TestWithSpatialObject)
{
  this->m_Parameters[0] = 0.5;
  this->m_Parameters[1] = 0.5;
  this->m_Parameters[2] = 0.25;
  this->m_Filter->SetParameters(this->m_Parameters);
  this->m_Filter->SetInput(this->m_NonZeroEigenImage);
  this->m_Filter->SetMask(this->m_SpatialObject);
  EXPECT_NO_THROW(this->m_Filter->Update());
  EXPECT_TRUE(this->m_Filter->GetOutput()->GetBufferedRegion() == this->m_Region);

  using ImageType = typename itk::Image<TypeParam, 3>;
  itk::ImageRegionIteratorWithIndex<ImageType> input(this->m_Filter->GetOutput(), this->m_Region);
  itk::ContinuousIndex<double, 3>              point;

  std::cout << this->m_MaskRegion << std::endl;

  input.GoToBegin();
  while (!input.IsAtEnd())
  {
    this->m_Filter->GetOutput()->TransformIndexToPhysicalPoint(input.GetIndex(), point);
    if (this->m_MaskRegion.IsInside(point))
    {
      ASSERT_NEAR((TypeParam)0.0913983433747, input.Get(), 1e-6);
    }
    else
    {
      ASSERT_NEAR((TypeParam)0.0, input.Get(), 1e-6);
    }
    ++input;
  }
}
