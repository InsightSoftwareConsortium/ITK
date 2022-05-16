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

//

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"

#include "itkGrayscaleFillholeImageFilter.h"
#include "itkXorImageFilter.h"
#include "itkNotImageFilter.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkTestingMacros.h"

int
itkRemoveBoundaryObjectsTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << "  inputImageFile  ";
    std::cerr << " outputImageFile  " << std::endl;
    return EXIT_FAILURE;
  }


  //
  //  The following code defines the input and output pixel types and their
  //  associated image types.
  //
  constexpr unsigned int Dimension = 2;

  using InputPixelType = unsigned char;
  using OutputPixelType = unsigned char;
  using WritePixelType = unsigned char;

  using InputImageType = itk::Image<InputPixelType, Dimension>;
  using OutputImageType = itk::Image<OutputPixelType, Dimension>;
  using WriteImageType = itk::Image<WritePixelType, Dimension>;


  // readers/writers
  using ReaderType = itk::ImageFileReader<InputImageType>;
  using WriterType = itk::ImageFileWriter<WriteImageType>;
  using RescaleType = itk::RescaleIntensityImageFilter<OutputImageType, WriteImageType>;

  // define the fillhole filter
  using FillholeFilterType = itk::GrayscaleFillholeImageFilter<InputImageType, OutputImageType>;

  // define the xor and not filters
  using XorFilterType = itk::XorImageFilter<InputImageType, InputImageType, OutputImageType>;
  using NotFilterType = itk::NotImageFilter<InputImageType, OutputImageType>;


  // Creation of Reader and Writer filters
  auto reader = ReaderType::New();
  auto writer = WriterType::New();
  auto rescaler = RescaleType::New();

  // Create the filter
  auto fillhole = FillholeFilterType::New();

  // Create the xor and not filter
  auto xorfilter = XorFilterType::New();
  auto notfilter = NotFilterType::New();

  // Setup the input and output files
  reader->SetFileName(argv[1]);
  writer->SetFileName(argv[2]);

  // Setup the fillhole method
  fillhole->SetInput(reader->GetOutput());

  // Setup the xor and not
  xorfilter->SetInput1(fillhole->GetOutput());
  xorfilter->SetInput2(reader->GetOutput());

  notfilter->SetInput(xorfilter->GetOutput());

  // Run the filter
  rescaler->SetInput(notfilter->GetOutput());
  rescaler->SetOutputMinimum(0);
  rescaler->SetOutputMaximum(255);
  writer->SetInput(rescaler->GetOutput());
  writer->Update();

  return EXIT_SUCCESS;
}
