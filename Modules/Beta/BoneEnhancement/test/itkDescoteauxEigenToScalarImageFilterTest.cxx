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

#include "itkDescoteauxEigenToScalarImageFilter.h"
#include "itkTestingMacros.h"
#include "itkMath.h"
#include "itkImageRegionIteratorWithIndex.h"

int
itkDescoteauxEigenToScalarImageFilterTest(int argc, char * argv[])
{
  constexpr unsigned int Dimension = 3;
  using MaskPixelType = unsigned int;
  using MaskType = itk::Image<MaskPixelType, Dimension>;

  using OutputPixelType = double;
  using OutputType = itk::Image<OutputPixelType, Dimension>;

  using EigenValueType = float;
  using EigenValueArrayType = itk::FixedArray<EigenValueType, Dimension>;
  using EigenValueImageType = itk::Image<EigenValueArrayType, Dimension>;

  using DescoteauxEigenToScalarImageFilterType =
    itk::DescoteauxEigenToScalarImageFilter<EigenValueImageType, OutputType, MaskType>;

  DescoteauxEigenToScalarImageFilterType::Pointer descoFilter = DescoteauxEigenToScalarImageFilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(descoFilter, DescoteauxEigenToScalarImageFilter, EigenToScalarImageFilter);

  /* Test defaults */
  ITK_TEST_EXPECT_EQUAL(descoFilter->GetEigenValueOrder(),
                        DescoteauxEigenToScalarImageFilterType::Superclass::OrderByMagnitude);

  /* Create some test data which is computable */
  EigenValueArrayType simpleEigenPixel;
  simpleEigenPixel.SetElement(0, 0.5);
  simpleEigenPixel.SetElement(1, 1.25);
  simpleEigenPixel.SetElement(2, -2);

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
  descoFilter->SetEnhanceBrightObjects();
  descoFilter->SetInput(image);
  ITK_TRY_EXPECT_NO_EXCEPTION(descoFilter->Update());

  /* Make sure process for setting parameters works */
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(descoFilter->GetAlpha(), 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(descoFilter->GetBeta(), 0.5, 6, 0.000001));
  ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(descoFilter->GetC(), 2.41091269 * 0.5, 6, 0.000001));

  itk::ImageRegionIteratorWithIndex<OutputType> it(descoFilter->GetOutput(), region);

  it.GoToBegin();
  while (!it.IsAtEnd())
  {
    ITK_TEST_EXPECT_TRUE(itk::Math::FloatAlmostEqual(it.Get(), 0.364376944099, 6, 0.000001));
    ++it;
  }

  return EXIT_SUCCESS;
}
