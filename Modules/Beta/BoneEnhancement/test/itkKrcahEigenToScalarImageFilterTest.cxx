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

#include "itkKrcahEigenToScalarImageFilter.h"
#include "itkTestingMacros.h"
#include "itkMath.h"
#include "itkImageRegionIteratorWithIndex.h"

int
itkKrcahEigenToScalarImageFilterTest(int argc, char * argv[])
{
  constexpr unsigned int Dimension = 3;
  using MaskPixelType = unsigned int;
  using MaskType = itk::Image<MaskPixelType, Dimension>;

  using OutputPixelType = double;
  using OutputType = itk::Image<OutputPixelType, Dimension>;

  using EigenValueType = float;
  using EigenValueArrayType = itk::FixedArray<EigenValueType, Dimension>;
  using EigenValueImageType = itk::Image<EigenValueArrayType, Dimension>;

  using KrcahEigenToScalarImageFilterType =
    itk::KrcahEigenToScalarImageFilter<EigenValueImageType, OutputType, MaskType>;

  KrcahEigenToScalarImageFilterType::Pointer krcahFilter = KrcahEigenToScalarImageFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(krcahFilter, KrcahEigenToScalarImageFilter, EigenToScalarImageFilter);

  /* Test defaults */
  ITK_TEST_EXPECT_EQUAL(krcahFilter->GetEigenValueOrder(),
                        KrcahEigenToScalarImageFilterType::Superclass::EigenValueOrderEnum::OrderByMagnitude);

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

  /* Run with no exceptions */
  krcahFilter->SetParameterSetToImplementation();
  krcahFilter->SetEnhanceBrightObjects();
  krcahFilter->SetInput(image);
  ITK_TRY_EXPECT_NO_EXCEPTION(krcahFilter->Update());

  /* Make sure process for setting parameters works */
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(krcahFilter->GetAlpha(), itk::Math::sqrt2 * 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(krcahFilter->GetBeta(), itk::Math::sqrt2 * 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(krcahFilter->GetGamma(), itk::Math::sqrt2 * 3 * 0.5, 6, 0.000001));

  itk::ImageRegionIteratorWithIndex<OutputType> it(krcahFilter->GetOutput(), region);

  it.GoToBegin();
  while (!it.IsAtEnd())
  {
    ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(it.Get(), 0.0158368867121, 6, 0.000001));
    ++it;
  }

  return EXIT_SUCCESS;
}
