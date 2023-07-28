/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 *
 *=========================================================================*/

#include <iostream>
#include "itkBilateralImageFilter.h"
#include "itkNullImageToImageFilterDriver.hxx"
#include "itkTestingMacros.h"

int
itkBilateralImageFilterTest(int, char *[])
{
  constexpr unsigned int Dimension = 2;

  using PixelType = float;
  using ImageType = itk::Image<PixelType, Dimension>;

  using FilterType = itk::BilateralImageFilter<ImageType, ImageType>;

  // Set up filter
  auto filter = FilterType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(filter, BilateralImageFilter, ImageToImageFilter);


  double                         domainSigmaVal = 2.0;
  typename FilterType::ArrayType domainSigma = FilterType::ArrayType::Filled(domainSigmaVal);
  filter->SetDomainSigma(domainSigmaVal);
  ITK_TEST_SET_GET_VALUE(domainSigma, filter->GetDomainSigma());

  domainSigmaVal = 2.5;
  domainSigma.Fill(domainSigmaVal);
  filter->SetDomainSigma(domainSigma);
  ITK_TEST_SET_GET_VALUE(domainSigma, filter->GetDomainSigma());

  double domainMu = 2.5;
  filter->SetDomainMu(domainMu);
  ITK_TEST_SET_GET_VALUE(domainMu, filter->GetDomainMu());

  double rangeSigma = 35.0f;
  filter->SetRangeSigma(rangeSigma);
  ITK_TEST_SET_GET_VALUE(rangeSigma, filter->GetRangeSigma());

  filter->SetFilterDimensionality(Dimension);
  ITK_TEST_SET_GET_VALUE(Dimension, filter->GetFilterDimensionality());

  bool automaticKernelSize = true;
  ITK_TEST_SET_GET_BOOLEAN(filter, AutomaticKernelSize, automaticKernelSize);

  typename FilterType::SizeType::SizeValueType radiusVal = 2;
  typename FilterType::SizeType                radius = FilterType::SizeType::Filled(radiusVal);
  filter->SetRadius(radius);
  ITK_TEST_SET_GET_VALUE(radius, filter->GetRadius());

  unsigned long numberOfRangeGaussianSamples = 150;
  filter->SetNumberOfRangeGaussianSamples(numberOfRangeGaussianSamples);
  ITK_TEST_SET_GET_VALUE(numberOfRangeGaussianSamples, filter->GetNumberOfRangeGaussianSamples());

  // Run test
  itk::Size<Dimension> sz;
  sz[0] = 250;
  sz[1] = 250;
  itk::NullImageToImageFilterDriver<ImageType, ImageType> test1;
  test1.SetImageSize(sz);
  test1.SetFilter(filter);

  ITK_TRY_EXPECT_NO_EXCEPTION(test1.Execute());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
