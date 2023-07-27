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
#include "itkTestingMacros.h"


int
itkBilateralImageFilterTest3(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " InputImage BaselineImage" << std::endl;
    return EXIT_FAILURE;
  }

  using PixelType = unsigned char;
  using myImage = itk::Image<PixelType, 2>;
  itk::ImageFileReader<myImage>::Pointer input = itk::ImageFileReader<myImage>::New();
  input->SetFileName(argv[1]);

  // Create a filter
  using FilterType = itk::BilateralImageFilter<myImage, myImage>;

  auto filter1 = FilterType::New();
  filter1->SetInput(input->GetOutput());
  auto filter2 = FilterType::New();
  filter2->SetInput(filter1->GetOutput());
  auto filter3 = FilterType::New();
  filter3->SetInput(filter2->GetOutput());

  // Instead of using a single aggressive smoothing filter, use 3
  // less aggressive filters.
  //
  // These settings match the "wedding" cake image (cake_easy.png) where
  // the signal to noise ratio is 5 (step heights near 100 units,
  // noise sigma near 20 units). A single filter stage with these
  // settings cuts the noise level in half.  These three stages should
  // reduce the amount of noise by a factor of 8. This is comparable to
  // the noise reduction in using a single stage with parameters
  // (4.0, 50.0).  The difference is that with 3 less aggressive stages
  // the edges are preserved better.
  auto domainSigma = 4.0;
  filter1->SetDomainSigma(domainSigma);
  auto rangeSigma = 20.0;
  filter1->SetRangeSigma(rangeSigma);
  auto domainMu = 2.5;
  filter1->SetDomainMu(domainMu);

  filter2->SetDomainSigma(domainSigma);
  filter2->SetRangeSigma(rangeSigma);
  filter2->SetDomainMu(domainMu);

  filter3->SetDomainSigma(domainSigma);
  filter3->SetRangeSigma(rangeSigma);
  filter3->SetDomainMu(domainMu);

  ITK_TRY_EXPECT_NO_EXCEPTION(input->Update());


  ITK_TRY_EXPECT_NO_EXCEPTION(filter3->Update());


  // Generate test image
  itk::ImageFileWriter<myImage>::Pointer writer;
  writer = itk::ImageFileWriter<myImage>::New();
  writer->SetInput(filter3->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
