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

#include "itkJPEGImageIO.h"
#include "itkImageFileReader.h"
#include "itkTestingMacros.h"


int
itkJPEGImageIOBrokenCasesTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " inputFilename" << std::endl;
    return EXIT_FAILURE;
  }

  constexpr unsigned int Dimension = 2;
  using PixelType = unsigned char;

  using ImageType = itk::Image<PixelType, Dimension>;

  itk::JPEGImageIO::Pointer io = itk::JPEGImageIO::New();

  itk::ImageFileReader<ImageType>::Pointer reader = itk::ImageFileReader<ImageType>::New();

  reader->SetFileName(argv[1]);
  reader->SetImageIO(io);

  ITK_TRY_EXPECT_EXCEPTION(reader->Update());


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
