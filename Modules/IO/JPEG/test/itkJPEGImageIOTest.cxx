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
#include "itkImageFileReader.h"
#include "itkImageFileWriter.h"
#include "itkTestingMacros.h"


// Specific ImageIO test

int
itkJPEGImageIOTest(int argc, char * argv[])
{

  if (argc < 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputFilename outputFilename" << std::endl;
    return EXIT_FAILURE;
  }

  // ATTENTION THIS IS THE PIXEL TYPE FOR
  // THE RESULTING IMAGE
  constexpr unsigned int Dimension = 2;
  using PixelType = unsigned char;

  using myImage = itk::Image<PixelType, Dimension>;

  itk::ImageFileReader<myImage>::Pointer reader = itk::ImageFileReader<myImage>::New();

  reader->SetFileName(argv[1]);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  myImage::Pointer image = reader->GetOutput();

  image->Print(std::cout);

  myImage::RegionType region = image->GetLargestPossibleRegion();
  std::cout << "region " << region;

  // Generate test image
  itk::ImageFileWriter<myImage>::Pointer writer;
  writer = itk::ImageFileWriter<myImage>::New();
  writer->SetInput(reader->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  return EXIT_SUCCESS;
}
