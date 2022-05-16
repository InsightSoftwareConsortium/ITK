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
#include "itkStimulateImageIO.h"
#include "itkTestingMacros.h"

#include <fstream>


// Specific ImageIO test

int
itkStimulateImageIOTest2(int argc, char * argv[])
{
  // This test is usually run with the data file
  // Insight/Testing/Data/Input/BigEndian.spr
  if (argc < 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " filename" << std::endl;
    return EXIT_FAILURE;
  }

  using PixelType = float;
  using myImage = itk::Image<PixelType, 3>;

  itk::StimulateImageIO::Pointer io;
  io = itk::StimulateImageIO::New();

  itk::ImageFileReader<myImage>::Pointer reader = itk::ImageFileReader<myImage>::New();

  std::cout << "Filename: " << argv[1] << std::endl;
  reader->SetFileName(argv[1]);
  reader->SetImageIO(io);

  ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());


  myImage::Pointer image = reader->GetOutput();
  image->Print(std::cout);

  myImage::RegionType region = image->GetLargestPossibleRegion();
  std::cout << "region " << region;

  // This is where we call all of the Get Functions to increase coverage.
  std::cout << "DisplayRange:" << io->GetDisplayRange() << std::endl;
  std::cout << "HighDisplayValue:" << io->GetHighDisplayValue() << std::endl;
  std::cout << "LowDisplayValue: " << io->GetLowDisplayValue() << std::endl;


  return EXIT_SUCCESS;
}
