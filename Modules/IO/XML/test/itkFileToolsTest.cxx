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

#include "itkFileTools.h"
#include "itkTestingMacros.h"
#include <iostream>

int
itkFileToolsTest(int argc, char * argv[])
{
  if (argc != 3)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv);
    std::cerr << " directoryName filename" << std::endl;
    return EXIT_FAILURE;
  }

  std::string directoryName = "";
  ITK_TRY_EXPECT_EXCEPTION(itk::FileTools::CreateDirectory(directoryName));

  directoryName = argv[1];
  ITK_TRY_EXPECT_NO_EXCEPTION(itk::FileTools::CreateDirectory(directoryName));

  std::string filename = "";
  ITK_TRY_EXPECT_EXCEPTION(itk::FileTools::CreateFile(filename));
  ITK_TRY_EXPECT_EXCEPTION(itk::FileTools::CreateFile(directoryName));
  ITK_TRY_EXPECT_EXCEPTION(itk::FileTools::CreateDirectory(filename));

  filename = argv[2];
  ITK_TRY_EXPECT_NO_EXCEPTION(itk::FileTools::CreateFile(filename));

  ITK_TRY_EXPECT_EXCEPTION(itk::FileTools::CreateDirectory(filename));


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
