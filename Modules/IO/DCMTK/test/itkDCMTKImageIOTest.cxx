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

#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkDCMTKImageIO.h"
#include "itkRescaleIntensityImageFilter.h"
#include "itkTestingMacros.h"

#include <fstream>

// Specific ImageIO test

int
itkDCMTKImageIOTest(int argc, char * argv[])
{

  if (argc < 5)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv)
              << " DicomImage OutputDicomImage OutputImage RescalDicomImage" << std::endl;
    return EXIT_FAILURE;
  }

  using InputPixelType = short;
  using InputImageType = itk::Image<InputPixelType, 2>;
  using ReaderType = itk::ImageFileReader<InputImageType>;

  using ImageIOType = itk::DCMTKImageIO;
  auto dcmtkImageIO = ImageIOType::New();

  auto reader = ReaderType::New();
  reader->SetFileName(argv[1]);
  reader->SetImageIO(dcmtkImageIO);
  // reader->DebugOn();

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  // Rescale intensities and rewrite the image in another format
  //
  using WritePixelType = unsigned char;
  using WriteImageType = itk::Image<WritePixelType, 2>;
  using RescaleFilterType = itk::RescaleIntensityImageFilter<InputImageType, WriteImageType>;

  auto rescaler = RescaleFilterType::New();
  rescaler->SetOutputMinimum(0);
  rescaler->SetOutputMaximum(255);
  rescaler->SetInput(reader->GetOutput());

  using Writer2Type = itk::ImageFileWriter<WriteImageType>;
  auto writer2 = Writer2Type::New();
  // writer2->DebugOn();
  writer2->SetFileName(argv[3]);
  writer2->SetInput(rescaler->GetOutput());

  ITK_TRY_EXPECT_NO_EXCEPTION(writer2->Update());


  dcmtkImageIO->Print(std::cout);

  // Test streaming enumeration for DCMTKImageIOEnums::LogLevel elements
  const std::set<itk::DCMTKImageIOEnums::LogLevel> allLogLevel{
    itk::DCMTKImageIOEnums::LogLevel::TRACE_LOG_LEVEL, itk::DCMTKImageIOEnums::LogLevel::DEBUG_LOG_LEVEL,
    itk::DCMTKImageIOEnums::LogLevel::INFO_LOG_LEVEL,  itk::DCMTKImageIOEnums::LogLevel::WARN_LOG_LEVEL,
    itk::DCMTKImageIOEnums::LogLevel::ERROR_LOG_LEVEL, itk::DCMTKImageIOEnums::LogLevel::FATAL_LOG_LEVEL,
    itk::DCMTKImageIOEnums::LogLevel::OFF_LOG_LEVEL
  };
  for (const auto & ee : allLogLevel)
  {
    std::cout << "STREAMED ENUM VALUE DCMTKImageIOEnums::LogLevel: " << ee << std::endl;
  }
  return EXIT_SUCCESS;
}
