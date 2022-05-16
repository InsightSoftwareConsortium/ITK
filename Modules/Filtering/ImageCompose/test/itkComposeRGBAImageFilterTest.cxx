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

// General includes
#include <iostream>

// ITK includes
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkComposeImageFilter.h"
#include "itkTestingMacros.h"

int
itkComposeRGBAImageFilterTest(int argc, char * argv[])
{

  if (argc < 6)
  {
    std::cerr << "Error: missing arguments" << std::endl;
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << " outputFile inputFileR inputFileG inputFileB inputFileA"
              << std::endl;
  }

  try
  {
    // ARGUMENTS:
    // argv[0] = Executable name
    // argv[1] = Output file name and path
    // argv[2] = Input 1 file name and path
    // argv[2] = Input 2 file name and path
    // argv[2] = Input 3 file name and path
    // argv[2] = Input 4 file name and path

    // Get arguments
    char * OutputFilename = argv[1];
    char * Input1Filename = argv[2];
    char * Input2Filename = argv[3];
    char * Input3Filename = argv[4];
    char * Input4Filename = argv[5];

    // Typedefs
    using ScalarPixelType = unsigned char;
    constexpr unsigned int Dimension = 2;
    using RGBAPixelType = itk::RGBAPixel<ScalarPixelType>;
    using ScalarImageType = itk::Image<ScalarPixelType, Dimension>;
    using RGBAImageType = itk::Image<RGBAPixelType, Dimension>;
    using ReaderType = itk::ImageFileReader<ScalarImageType>;
    using WriterType = itk::ImageFileWriter<RGBAImageType>;
    using ComposeFilterType = itk::ComposeImageFilter<ScalarImageType, RGBAImageType>;

    // Read input1
    auto reader1 = ReaderType::New();
    reader1->SetFileName(Input1Filename);
    reader1->Update();

    // Read input2
    auto reader2 = ReaderType::New();
    reader2->SetFileName(Input2Filename);
    reader2->Update();

    // Read input3
    auto reader3 = ReaderType::New();
    reader3->SetFileName(Input3Filename);
    reader3->Update();

    // Read input4
    auto reader4 = ReaderType::New();
    reader4->SetFileName(Input4Filename);
    reader4->Update();

    // Test ComposeRGBA filter
    auto filterCompose = ComposeFilterType::New();
    filterCompose->SetInput(0, reader1->GetOutput());
    filterCompose->SetInput(1, reader2->GetOutput());
    filterCompose->SetInput(2, reader3->GetOutput());
    filterCompose->SetInput(3, reader4->GetOutput());
    filterCompose->Update();

    // Write output
    auto writer = WriterType::New();
    writer->SetFileName(OutputFilename);
    writer->SetInput(filterCompose->GetOutput());
    writer->Update();
  }
  catch (const itk::ExceptionObject & err)
  {
    std::cerr << "ExceptionObject caught !" << std::endl;
    std::cerr << err << std::endl;
    return EXIT_FAILURE;
  }

  // Return
  return EXIT_SUCCESS;
}
