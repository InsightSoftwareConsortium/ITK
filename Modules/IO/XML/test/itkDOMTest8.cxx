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

/*
This program tests operations of itk::FileTools.
*/

#include "itkFileTools.h"

#include <iostream>
#include <string>
#include "itkMacro.h"
#include "itkTestingMacros.h"

int
itkDOMTest8(int argc, char * argv[])
{
  if (argc < 3)
  {
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " <OutputFolder> <OutputFile>" << std::endl;
    return EXIT_FAILURE;
  }

  try
  {
    // create the directory
    itk::FileTools::CreateDirectory(argv[1]);

    // create the file in the directory
    std::string fn(argv[1]);
    fn.append(argv[2]);
    itk::FileTools::CreateFile(fn);

    // the testings are successful if reached here
  }
  catch (const itk::ExceptionObject & eo)
  {
    eo.Print(std::cerr);
    return EXIT_FAILURE;
  }
  catch (...)
  {
    std::cerr << "Unknown exception caught!" << std::endl;
    return EXIT_FAILURE;
  }

  return EXIT_SUCCESS;
}
