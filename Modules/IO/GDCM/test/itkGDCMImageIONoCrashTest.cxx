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
#include "itkGDCMImageIO.h"
#include "itkTestingMacros.h"

#include <fstream>


// Specific ImageIO test

int
itkGDCMImageIONoCrashTest(int argc, char * argv[])
{

  if (argc < 2)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " DicomImage\n";
    return EXIT_FAILURE;
  }


  using InputPixelType = unsigned char;
  using InputImageType = itk::Image<InputPixelType, 2>;
  using ReaderType = itk::ImageFileReader<InputImageType>;

  using ImageIOType = itk::GDCMImageIO;
  auto gdcmImageIO = ImageIOType::New();

  const std::string inputFile{ argv[1] };

  auto reader = ReaderType::New();
  reader->SetFileName(inputFile);
  reader->SetImageIO(gdcmImageIO);
  if (!gdcmImageIO->CanReadFile(inputFile.c_str()))
  {
    std::cerr << "exception in file reader " << inputFile << " not supported by itk::GDCMImageIO." << '\n';
    return EXIT_FAILURE;
  }

  try
  {
    reader->Update();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "exception in file reader " << '\n';
    std::cerr << e << '\n';
    return EXIT_FAILURE;
  }

  // Exercise the get methods
  std::cout << "InternalComponentType: " << gdcmImageIO->GetInternalComponentType() << '\n';
  std::cout << "RescaleSlope: " << gdcmImageIO->GetRescaleSlope() << '\n';
  std::cout << "RescaleIntercept: " << gdcmImageIO->GetRescaleIntercept() << '\n';
  std::cout << "UIDPrefix: " << gdcmImageIO->GetUIDPrefix() << '\n';
  std::cout << "StudyInstanceUID: " << gdcmImageIO->GetStudyInstanceUID() << '\n';
  std::cout << "SeriesInstanceUID: " << gdcmImageIO->GetSeriesInstanceUID() << '\n';
  std::cout << "FrameOfReferenceInstanceUID: " << gdcmImageIO->GetFrameOfReferenceInstanceUID() << '\n';
  std::cout << "KeepOriginalUID: " << gdcmImageIO->GetKeepOriginalUID() << '\n';
  std::cout << "LoadPrivateTags: " << gdcmImageIO->GetLoadPrivateTags() << '\n';
  std::cout << "CompressionType: " << gdcmImageIO->GetCompressionType() << '\n';

  gdcmImageIO->Print(std::cout);

  return EXIT_SUCCESS;
}
