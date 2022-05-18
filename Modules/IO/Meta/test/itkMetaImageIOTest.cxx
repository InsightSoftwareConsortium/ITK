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
#include "itkMetaImageIO.h"
#include "itkTestingMacros.h"


// Specific ImageIO test

int
itkMetaImageIOTest(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Missing Parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " Input Output [expectFailureReadingInputFile]"
              << std::endl;
    return EXIT_FAILURE;
  }

  // ATTENTION THIS IS THE PIXEL TYPE FOR
  // THE RESULTING IMAGE
  using PixelType = unsigned short;
  using myImage = itk::Image<PixelType, 3>;

  itk::ImageFileReader<myImage>::Pointer reader = itk::ImageFileReader<myImage>::New();

  // Force use of MetaIO
  using IOType = itk::MetaImageIO;
  auto metaIn = IOType::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(metaIn, MetaImageIO, ImageIOBase);


  metaIn->SetDoublePrecision(8); // Set manually for coverage
  reader->SetImageIO(metaIn);

  // Check usability of dimension (for coverage)
  if (!metaIn->SupportsDimension(3))
  {
    std::cerr << "Did not support dimension 3" << std::endl;
    return EXIT_FAILURE;
  }

  // Test subsampling factor (change it then change it back)
  unsigned int origSubSamplingFactor = metaIn->GetSubSamplingFactor();
  unsigned int subSamplingFactor = 2;
  metaIn->SetSubSamplingFactor(subSamplingFactor);
  ITK_TEST_SET_GET_VALUE(subSamplingFactor, metaIn->GetSubSamplingFactor());

  metaIn->SetSubSamplingFactor(origSubSamplingFactor);

  // Read the file
  reader->SetFileName(argv[1]);

  auto expectFailureReadingInputFile = false;
  if (argc > 3)
  {
    expectFailureReadingInputFile = static_cast<bool>(std::stoi(argv[3]));
  }

  if (expectFailureReadingInputFile)
  {
    ITK_TRY_EXPECT_EXCEPTION(reader->Update());
    return EXIT_SUCCESS;
  }
  else
  {
    ITK_TRY_EXPECT_NO_EXCEPTION(reader->Update());
  }

  myImage::Pointer image = reader->GetOutput();
  image->Print(std::cout);

  myImage::RegionType region = image->GetLargestPossibleRegion();
  std::cout << "region " << region;

  // Generate test image
  itk::ImageFileWriter<myImage>::Pointer writer;
  writer = itk::ImageFileWriter<myImage>::New();
  auto metaOut = IOType::New();
  writer->SetImageIO(metaOut);
  writer->SetInput(reader->GetOutput());
  writer->SetFileName(argv[2]);

  ITK_TRY_EXPECT_NO_EXCEPTION(writer->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
