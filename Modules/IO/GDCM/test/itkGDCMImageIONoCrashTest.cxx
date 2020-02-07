/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         http://www.apache.org/licenses/LICENSE-2.0.txt
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

#include <fstream>


// Specific ImageIO test

int
itkGDCMImageIONoCrashTest(int ac, char * av[])
{

  if (ac < 2)
  {
    std::cerr << "Usage: " << av[0] << " DicomImage\n";
    return EXIT_FAILURE;
  }


  using InputPixelType = unsigned char;
  using InputImageType = itk::Image<InputPixelType, 2>;
  using ReaderType = itk::ImageFileReader<InputImageType>;

  using ImageIOType = itk::GDCMImageIO;
  ImageIOType::Pointer gdcmImageIO = ImageIOType::New();

  const std::string inputFile{ av[1] };

  ReaderType::Pointer reader = ReaderType::New();
  reader->SetFileName(inputFile);
  reader->SetImageIO(gdcmImageIO);
  if (!gdcmImageIO->CanReadFile(inputFile.c_str()))
  {
    std::cerr << "exception in file reader " << inputFile << " not supported by itk::GDCMImageIO." << std::endl;
    return EXIT_FAILURE;
  }

  try
  {
    reader->Update();
  }
  catch (const itk::ExceptionObject & e)
  {
    std::cerr << "exception in file reader " << std::endl;
    std::cerr << e << std::endl;
    return EXIT_FAILURE;
  }

  // Exercise the get methods
  std::cout << "InternalComponentType: " << gdcmImageIO->GetInternalComponentType() << std::endl;
  std::cout << "RescaleSlope: " << gdcmImageIO->GetRescaleSlope() << std::endl;
  std::cout << "RescaleIntercept: " << gdcmImageIO->GetRescaleIntercept() << std::endl;
  std::cout << "UIDPrefix: " << gdcmImageIO->GetUIDPrefix() << std::endl;
  std::cout << "StudyInstanceUID: " << gdcmImageIO->GetStudyInstanceUID() << std::endl;
  std::cout << "SeriesInstanceUID: " << gdcmImageIO->GetSeriesInstanceUID() << std::endl;
  std::cout << "FrameOfReferenceInstanceUID: " << gdcmImageIO->GetFrameOfReferenceInstanceUID() << std::endl;
  std::cout << "KeepOriginalUID: " << gdcmImageIO->GetKeepOriginalUID() << std::endl;
#ifndef ITK_LEGACY_REMOVE
  std::cout << "LoadSequences: " << gdcmImageIO->GetLoadSequences() << std::endl;
#endif
  std::cout << "LoadPrivateTags: " << gdcmImageIO->GetLoadPrivateTags() << std::endl;
  std::cout << "CompressionType: " << gdcmImageIO->GetCompressionType() << std::endl;

  gdcmImageIO->Print(std::cout);

  return EXIT_SUCCESS;
}
