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

#include <iostream>
#include <fstream>
#include "itkStdStreamLogOutput.h"
#include "itkMultipleLogOutput.h"
#include "itkTestingMacros.h"


int
itkMultipleLogOutputTest(int argc, char * argv[])
{
  try
  {
    if (argc < 2)
    {
      std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << " logFilename" << std::endl;
      return EXIT_FAILURE;
    }


    // Create an ITK StdStreamLogOutput
    const itk::StdStreamLogOutput::Pointer coutput = itk::StdStreamLogOutput::New();
    const itk::StdStreamLogOutput::Pointer foutput = itk::StdStreamLogOutput::New();
    const itk::MultipleLogOutput::Pointer  output = itk::MultipleLogOutput::New();

    std::cout << "Testing itk::MultipleLogOutput" << std::endl;
    coutput->SetStream(std::cout);
    std::ofstream fout(argv[1]);
    foutput->SetStream(fout);

    std::cout << "  Adding console and file stream LogOutputs" << std::endl;
    output->AddLogOutput(coutput);
    output->AddLogOutput(foutput);

    std::cout << "  Writing by itk::MultipleLogOutput" << std::endl;
    output->Write(1.2345);
    output->Write("This is the test message.\n");
    output->Write("This is the second test message.\n", 1.2345);
    output->Flush();
  }
  catch (...)
  {
    std::cerr << "Exception caught !!" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
