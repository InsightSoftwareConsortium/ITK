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

#include "itkKrcahEigenToScalarParameterEstimationImageFilter.h"
#include "itkTestingMacros.h"
#include "itkMath.h"
#include "itkImageRegionIteratorWithIndex.h"

int
itkKrcahEigenToScalarParameterEstimationImageFilterTest(int, char *[])
{
  constexpr unsigned int Dimension = 3;
  using MaskPixelType = unsigned int;
  using MaskType = itk::Image<MaskPixelType, Dimension>;

  using EigenValueType = float;
  using EigenValueArrayType = itk::FixedArray<EigenValueType, Dimension>;
  using EigenValueImageType = itk::Image<EigenValueArrayType, Dimension>;

  using KrcahEigenToScalarParameterEstimationImageFilterType =
    itk::KrcahEigenToScalarParameterEstimationImageFilter<EigenValueImageType, MaskType>;

  KrcahEigenToScalarParameterEstimationImageFilterType::Pointer krcahParameterEstimator =
    KrcahEigenToScalarParameterEstimationImageFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(
    krcahParameterEstimator, KrcahEigenToScalarParameterEstimationImageFilter, ImageToImageFilter);

  /* Test defaults */
  ITK_TEST_EXPECT_EQUAL(krcahParameterEstimator->GetParameterSet(),
                        KrcahEigenToScalarParameterEstimationImageFilterType::UseImplementationParameters);
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetAlpha(), 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetBeta(), 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetGamma(), 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetAlphaOutput()->Get(), 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetBetaOutput()->Get(), 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetGammaOutput()->Get(), 0.5, 6, 0.000001));

  /* Create some test data which is computable */
  EigenValueArrayType simpleEigenPixel;
  for (unsigned int i = 0; i < Dimension; ++i)
  {
    simpleEigenPixel.SetElement(i, -1);
  }

  EigenValueImageType::RegionType region;
  EigenValueImageType::IndexType  start;
  start[0] = 0;
  start[1] = 0;
  start[2] = 0;

  EigenValueImageType::SizeType size;
  size[0] = 10;
  size[1] = 10;
  size[2] = 10;

  region.SetSize(size);
  region.SetIndex(start);

  EigenValueImageType::Pointer image = EigenValueImageType::New();
  image->SetRegions(region);
  image->Allocate();
  image->FillBuffer(simpleEigenPixel);

  EigenValueImageType::Pointer image2 = EigenValueImageType::New();
  image2->SetRegions(region);
  image2->Allocate();
  image2->FillBuffer(simpleEigenPixel);

  EigenValueImageType::IndexType maskStart;
  maskStart[0] = 2;
  maskStart[1] = 2;
  maskStart[2] = 2;

  EigenValueImageType::SizeType maskSize;
  maskSize[0] = 8;
  maskSize[1] = 8;
  maskSize[2] = 8;

  MaskPixelType     backgroundValue = 1;
  MaskPixelType     foregroundValue = 2;
  MaskType::Pointer mask = MaskType::New();
  mask->SetRegions(region);
  mask->Allocate();
  mask->FillBuffer(backgroundValue);

  EigenValueArrayType newEigenPixel;
  for (unsigned int i = 0; i < Dimension; ++i)
  {
    newEigenPixel.SetElement(i, 3);
  }

  EigenValueImageType::RegionType maskRegion;
  maskRegion.SetSize(maskSize);
  maskRegion.SetIndex(maskStart);

  itk::ImageRegionIteratorWithIndex<EigenValueImageType> input2It(image2, maskRegion);
  itk::ImageRegionIteratorWithIndex<MaskType>            maskIt(mask, maskRegion);

  input2It.GoToBegin();
  maskIt.GoToBegin();
  while (!input2It.IsAtEnd())
  {
    input2It.Set(newEigenPixel);
    maskIt.Set(foregroundValue);
    ++input2It;
    ++maskIt;
  }

  /* Test an empty pixel value */
  krcahParameterEstimator->SetInput(image);
  krcahParameterEstimator->SetParameterSetToImplementation();
  ITK_TRY_EXPECT_NO_EXCEPTION(krcahParameterEstimator->Update());
  ITK_TEST_EXPECT_TRUE(
    itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetAlpha(), itk::Math::sqrt2 * 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(
    itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetBeta(), itk::Math::sqrt2 * 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(
    itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetGamma(), itk::Math::sqrt2 * 3 * 0.5, 6, 0.000001));

  krcahParameterEstimator->SetParameterSetToJournalArticle();
  ITK_TRY_EXPECT_NO_EXCEPTION(krcahParameterEstimator->Update());
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetAlpha(), 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetBeta(), 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetGamma(), -3 * 0.25, 6, 0.000001));

  /* Test with a mask */
  krcahParameterEstimator->SetParameterSetToImplementation();
  krcahParameterEstimator->SetInput(image2);
  krcahParameterEstimator->SetMaskImage(mask);
  krcahParameterEstimator->SetBackgroundValue(backgroundValue);
  ITK_TRY_EXPECT_NO_EXCEPTION(krcahParameterEstimator->Update());
  ITK_TEST_EXPECT_TRUE(
    itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetAlpha(), itk::Math::sqrt2 * 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(
    itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetBeta(), itk::Math::sqrt2 * 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(
    itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetGamma(), itk::Math::sqrt2 * 3 * 3 * 0.5, 6, 0.000001));

  krcahParameterEstimator->SetParameterSetToImplementation();
  krcahParameterEstimator->SetBackgroundValue(foregroundValue);
  ITK_TRY_EXPECT_NO_EXCEPTION(krcahParameterEstimator->Update());
  ITK_TEST_EXPECT_TRUE(
    itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetAlpha(), itk::Math::sqrt2 * 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(
    itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetBeta(), itk::Math::sqrt2 * 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(
    itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetGamma(), itk::Math::sqrt2 * 3 * 1 * 0.5, 6, 0.000001));

  krcahParameterEstimator->SetParameterSetToJournalArticle();
  krcahParameterEstimator->SetInput(image2);
  krcahParameterEstimator->SetMaskImage(mask);
  krcahParameterEstimator->SetBackgroundValue(backgroundValue);
  ITK_TRY_EXPECT_NO_EXCEPTION(krcahParameterEstimator->Update());
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetAlpha(), 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetBeta(), 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetGamma(), 3 * 3 * 0.25, 6, 0.000001));

  krcahParameterEstimator->SetParameterSetToJournalArticle();
  krcahParameterEstimator->SetBackgroundValue(foregroundValue);
  ITK_TRY_EXPECT_NO_EXCEPTION(krcahParameterEstimator->Update());
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetAlpha(), 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetBeta(), 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_EQUAL(krcahParameterEstimator->GetGamma(), -3 * 1 * 0.25);
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetGamma(), -3 * 1 * 0.25, 6, 0.000001));

  MaskType::Pointer mask2 = MaskType::New();
  mask2->SetRegions(maskRegion);
  mask2->Allocate();
  mask2->FillBuffer(foregroundValue);

  krcahParameterEstimator->SetInput(image);
  krcahParameterEstimator->SetParameterSetToImplementation();
  krcahParameterEstimator->SetMaskImage(mask2);
  krcahParameterEstimator->SetBackgroundValue(backgroundValue);
  ITK_TRY_EXPECT_NO_EXCEPTION(krcahParameterEstimator->Update());
  ITK_TEST_EXPECT_TRUE(
    itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetAlpha(), itk::Math::sqrt2 * 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(
    itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetBeta(), itk::Math::sqrt2 * 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(
    itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetGamma(), itk::Math::sqrt2 * 3 * 1 * 0.5, 6, 0.000001));

  krcahParameterEstimator->SetInput(image);
  krcahParameterEstimator->SetParameterSetToJournalArticle();
  krcahParameterEstimator->SetMaskImage(mask2);
  krcahParameterEstimator->SetBackgroundValue(backgroundValue);
  ITK_TRY_EXPECT_NO_EXCEPTION(krcahParameterEstimator->Update());
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetAlpha(), 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetBeta(), 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(krcahParameterEstimator->GetGamma(), -3 * 1 * 0.25, 6, 0.000001));

  return EXIT_SUCCESS;
}
