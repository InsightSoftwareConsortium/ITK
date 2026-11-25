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
#include "itkTIFFImageIO.h"
#include "itkTestingMacros.h"
#include <fstream>

int
itkTIFFImageIOIntPixelTest(int argc, char * argv[])
{
  if (argc < 2)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " <IntImage>.tif" << std::endl;
    return EXIT_FAILURE;
  }
  const char * fileName = argv[1];

  auto imageIO = itk::TIFFImageIO::New();
  imageIO->SetFileName(fileName);
  imageIO->ReadImageInformation();

  const auto componentType = imageIO->GetComponentType();
  if (componentType != itk::IOComponentEnum::INT)
  {
    std::cerr << "Expected identification as an int pixel type." << std::endl;
    return EXIT_FAILURE;
  }

  imageIO->Print(std::cout);

  return EXIT_SUCCESS;
}
