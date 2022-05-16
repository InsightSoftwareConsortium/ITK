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

#include "itkAccumulateImageFilter.h"
#include "itkImageSeriesReader.h"
#include "itkImageSeriesWriter.h"
#include "itkGDCMImageIO.h"
#include "itkGDCMSeriesFileNames.h"
#include "itkTestingMacros.h"

int
itkAccumulateImageFilterTest(int argc, char * argv[])
{
  using PixelType = short;
  static constexpr int ImageDimension = 3;

  using InputImageType = itk::Image<PixelType, ImageDimension>;
  using OutputImageType = itk::Image<PixelType, ImageDimension>;
  using WriteImageType = itk::Image<unsigned char, ImageDimension>;
  using ReaderType = itk::ImageSeriesReader<InputImageType>;
  using AccumulaterType = itk::AccumulateImageFilter<InputImageType, OutputImageType>;
  using WriterType = itk::ImageSeriesWriter<OutputImageType, WriteImageType>;
  using SeriesFileNames = itk::GDCMSeriesFileNames;
  using ImageIOType = itk::GDCMImageIO;

  if (argc < 3)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << "  inputDICOMDirectory outputFile" << std::endl;
    return EXIT_FAILURE;
  }

  // Get the input filenames
  auto names = SeriesFileNames::New();

  // Get the DICOM filenames from the directory
  names->SetInputDirectory(argv[1]);

  // Create the reader
  auto gdcmIO = ImageIOType::New();
  auto reader = ReaderType::New();
  reader->SetImageIO(gdcmIO);
  try
  {
    reader->SetFileNames(names->GetInputFileNames());
    reader->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Error reading the series" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  // Accumulate the input images
  auto accumulate = AccumulaterType::New();
  accumulate->SetInput(reader->GetOutput());
  accumulate->SetAccumulateDimension(2);

  try
  {
    accumulate->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Error running the accumulate filter" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }
  accumulate->GetOutput()->Print(std::cout);

  accumulate->Print(std::cout);

  // Now turn averaging off
  accumulate->AverageOff();
  try
  {
    auto writer = WriterType::New();
    writer->SetFileName(argv[2]);

    writer->SetInput(accumulate->GetOutput());
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Error writing the series" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  // Now turn averaging on
  accumulate->AverageOn();
  std::cout << "Average: " << accumulate->GetAverage() << std::endl;

  try
  {
    auto writer = WriterType::New();
    writer->SetFileName(argv[2]);

    writer->SetInput(accumulate->GetOutput());
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Error writing the series" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }

  // Test dimension check exception.
  try
  {
    accumulate->SetAccumulateDimension(5);
    accumulate->Update();
    std::cout << "Failed to catch expected exception." << std::endl;
    return EXIT_FAILURE;
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cout << "Caught expected exception." << std::endl;
    std::cout << excp << std::endl;
  }
  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
