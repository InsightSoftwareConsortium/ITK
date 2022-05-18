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

#include "itkGetAverageSliceImageFilter.h"
#include "itkImageSeriesReader.h"
#include "itkImageSeriesWriter.h"
#include "itkGDCMImageIO.h"
#include "itkGDCMSeriesFileNames.h"
#include "itkTestingMacros.h"

int
itkGetAverageSliceImageFilterTest(int argc, char * argv[])
{
  using PixelType = short;
  static constexpr int ImageDimension = 3;

  using InputImageType = itk::Image<PixelType, ImageDimension>;
  using OutputImageType = itk::Image<PixelType, ImageDimension>;
  using WriteImageType = itk::Image<unsigned char, ImageDimension>;
  using ReaderType = itk::ImageSeriesReader<InputImageType>;
  using GetAveragerType = itk::GetAverageSliceImageFilter<InputImageType, OutputImageType>;
  using WriterType = itk::ImageSeriesWriter<OutputImageType, WriteImageType>;
  using SeriesFileNames = itk::GDCMSeriesFileNames;
  using ImageIOType = itk::GDCMImageIO;

  if (argc < 4)
  {
    std::cerr << "Usage: " << std::endl;
    std::cerr << itkNameOfTestExecutableMacro(argv) << "  inputDICOMDirectory outputFile averagedOutDimension"
              << std::endl;
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

  // GetAverage the input images
  auto average = GetAveragerType::New();
  ITK_EXERCISE_BASIC_OBJECT_METHODS(average, GetAverageSliceImageFilter, AccumulateImageFilter);

  auto averagedOutDimension = static_cast<unsigned int>(std::stoi(argv[3]));
  average->SetAveragedOutDimension(averagedOutDimension);
  ITK_TEST_SET_GET_VALUE(averagedOutDimension, average->GetAveragedOutDimension());

  average->SetInput(reader->GetOutput());

  try
  {
    average->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Error running the average filter" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }
  average->GetOutput()->Print(std::cout);

  average->Print(std::cout);

  try
  {
    auto writer = WriterType::New();
    writer->SetFileName(argv[2]);

    writer->SetInput(average->GetOutput());
    writer->Update();
  }
  catch (const itk::ExceptionObject & excp)
  {
    std::cerr << "Error writing the series" << std::endl;
    std::cerr << excp << std::endl;
    return EXIT_FAILURE;
  }
  std::cout << "Test passed." << std::endl;
  return EXIT_SUCCESS;
}
