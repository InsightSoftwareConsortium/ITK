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

#include <fstream>
#include "itkBilateralImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkSimpleFilterWatcher.h"
#include "itkTestingMacros.h"

int
itkBilateralImageFilterTest2(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " InputImage OutputImage" << std::endl;
    return EXIT_FAILURE;
  }

  using PixelType = unsigned char;
  constexpr unsigned int dimension = 2;
  using myImage = itk::Image<PixelType, dimension>;
  itk::ImageFileReader<myImage>::Pointer input = itk::ImageFileReader<myImage>::New();
  input->SetFileName(argv[1]);

  // Create a filter
  using FilterType = itk::BilateralImageFilter<myImage, myImage>;

  auto                     filter = FilterType::New();
  itk::SimpleFilterWatcher watcher(filter, "filter");

  filter->SetInput(input->GetOutput());

  // these settings reduce the amount of noise by a factor of 10
  // when the original signal to noise level is 5
  auto domainSigmaValue = 4.0;
  filter->SetDomainSigma(domainSigmaValue);
  auto domainSigma = filter->GetDomainSigma();
  for (auto & value : domainSigma)
  {
    auto index = &value - &*(domainSigma.begin());
    ITK_TEST_SET_GET_VALUE(domainSigmaValue, filter->GetDomainSigma()[index]);
  }

  double domainSigmaArr[dimension];
  for (double & i : domainSigmaArr)
  {
    i = domainSigmaValue;
  }
  filter->SetDomainSigma(domainSigmaArr);
  for (auto & value : domainSigmaArr)
  {
    auto index = &value - &domainSigmaArr[0];
    ITK_TEST_SET_GET_VALUE(value, filter->GetDomainSigma()[index]);
  }

  auto rangeSigma = 50.0;
  filter->SetRangeSigma(rangeSigma);
  ITK_TEST_SET_GET_VALUE(rangeSigma, filter->GetRangeSigma());

  auto domainMu = 2.5;
  filter->SetDomainMu(domainMu);
  ITK_TEST_SET_GET_VALUE(domainMu, filter->GetDomainMu());

  unsigned int filterDimensionality = dimension;
  filter->SetFilterDimensionality(filterDimensionality);
  ITK_TEST_SET_GET_VALUE(filterDimensionality, filter->GetFilterDimensionality());

  unsigned long numberOfRangeGaussianSamples = 100;
  filter->SetNumberOfRangeGaussianSamples(numberOfRangeGaussianSamples);
  ITK_TEST_SET_GET_VALUE(numberOfRangeGaussianSamples, filter->GetNumberOfRangeGaussianSamples());


  ITK_TRY_EXPECT_NO_EXCEPTION(input->Update());


  ITK_TRY_EXPECT_NO_EXCEPTION(filter->Update());


  // Generate test image
  itk::ImageFileWriter<myImage>::Pointer writer;
  writer = itk::ImageFileWriter<myImage>::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
