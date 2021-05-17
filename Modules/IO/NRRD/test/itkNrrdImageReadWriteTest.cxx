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

#include <fstream>
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkNrrdImageIO.h"
#include "itkTestingMacros.h"

// Specific ImageIO test

int
itkNrrdImageReadWriteTest(int ac, char * av[])
{
  if (ac < 3)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(av) << " Input Output" << std::endl;
    return EXIT_FAILURE;
  }


  constexpr unsigned int Dimension = 3;

  using PixelType = float;
  using myImage = itk::Image<PixelType, Dimension>;

  using ReaderType = itk::ImageFileReader<myImage>;

  ReaderType::Pointer reader = ReaderType::New();

  reader->SetImageIO(itk::NrrdImageIO::New());

  reader->SetFileName(av[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  myImage::Pointer image = reader->GetOutput();

  image->Print(std::cout);

  // Generate test image
  itk::ImageFileWriter<myImage>::Pointer writer;
  writer = itk::ImageFileWriter<myImage>::New();
  writer->SetImageIO(itk::NrrdImageIO::New());
  writer->SetInput(reader->GetOutput());
  writer->SetFileName(av[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
