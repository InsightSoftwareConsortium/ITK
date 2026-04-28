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
#include "itkFastBilateralImageFilter.h"
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkImageRegionIterator.h"
#include "itkTestingMacros.h"

/**
 * This test was originally taken from the tests for the itkBilateralImageFilter
 * and modified for the itkFastBilateralImageFilter.
 */
int
itkFastBilateralImageFilterTest2(int ac, char * av[])
{
  if (ac < 3)
  {
    std::cerr << "Usage: " << av[0] << " InputImage OutputImage\n";
    return EXIT_FAILURE;
  }

  using PixelType = unsigned char;
  const unsigned int dimension = 2;
  using ImageType = itk::Image<PixelType, dimension>;
  auto input = itk::ImageFileReader<ImageType>::New();
  input->SetFileName(av[1]);

  // Create a filter
  using FilterType = itk::FastBilateralImageFilter<ImageType, ImageType>;

  auto filter = FilterType::New();

  filter->SetInput(input->GetOutput());

  // these settings reduce the amount of noise by a factor of 10
  // when the original signal to noise level is 5
  filter->SetDomainSigma(4.0);
  filter->SetRangeSigma(50.0);


  // Test itkSetVectorMacro
  double domainSigma[dimension];
  for (unsigned int i = 0; i < dimension; i++)
  {
    domainSigma[i] = 4.0;
  }
  filter->SetDomainSigma(domainSigma);

  // Test itkGetMacro
  std::cout << "filter->GetDomainSigma(): " << filter->GetDomainSigma() << std::endl;
  std::cout << "filter->GetRangeSigma(): " << filter->GetRangeSigma() << std::endl;

  try
  {
    input->Update();
    filter->Update();
  }
  catch (itk::ExceptionObject & e)
  {
    std::cerr << "Exception detected: " << e.GetDescription();
    return -1;
  }
  catch (...)
  {
    std::cerr << "Some other exception occurred" << std::endl;
    return -2;
  }

  // Generate test image
  itk::ImageFileWriter<ImageType>::Pointer writer;
  writer = itk::ImageFileWriter<ImageType>::New();
  writer->SetInput(filter->GetOutput());
  writer->SetFileName(av[2]);
  writer->Update();

  return EXIT_SUCCESS;
}
