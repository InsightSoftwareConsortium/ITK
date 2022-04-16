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


#include "itkXMLFileOutputWindow.h"
#include "itkTestingMacros.h"
#include <fstream>
#include <cstdio>
#ifdef _WIN32
#  include <direct.h>
#  define cwd _getcwd
#  define OS_SEP "\\"
#else
#  include "unistd.h"
#  define cwd getcwd
#  define OS_SEP "/"
#endif


int
itkXMLFileOutputWindowTest(int argc, char * argv[])
{
  if (argc < 1)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " [filename]" << std::endl;
    return EXIT_FAILURE;
  }

  itk::XMLFileOutputWindow::Pointer logger = itk::XMLFileOutputWindow::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(logger, XMLFileOutputWindow, FileOutputWindow);

  if (argc > 1)
  {
    logger->SetFileName(argv[1]);
  }

  logger->FlushOn();
  logger->AppendOn();

  logger->SetInstance(logger);

  // If not input filename is provided, remove the contents in the existing file to avoid counting existing lines when
  // contents are appended
  if (argc == 1)
  {
    // In order to initialize the filename, some text needs to be written first
    const char * regularText = "text";
    logger->DisplayText(regularText);

    // Get the filename
    const char *      fileBaseName = logger->GetFileName();
    const std::size_t size = 4096;
    char              tmp[size];
    char *            status = cwd(tmp, size);
    if (!status)
    {
      std::cerr << "Test failed!" << std::endl;
      std::cerr << "Error getting the current directory. Cannot delete the contents of the target file: "
                << fileBaseName << std::endl;
      return EXIT_FAILURE;
    }
    std::string fileName = tmp + std::string(OS_SEP) + std::string(fileBaseName);

    // Delete the contents
    std::ofstream ofs;
    ofs.open(fileName, std::ofstream::out | std::ofstream::trunc);
    ofs.close();
  }

  // Check special cases
  const char * text = nullptr;
  logger->DisplayText(text);

  const char * specialCharText = "text&\"'<>";
  logger->DisplayText(specialCharText);

  // Regular cases
  const char * regularText = "text";
  logger->DisplayText(regularText);

  const char * errorText = "errorText";
  logger->DisplayErrorText(errorText);

  const char * warningText = "warningText";
  logger->DisplayWarningText(warningText);

  const char * genericOutputText = "genericOutputText";
  logger->DisplayGenericOutputText(genericOutputText);

  const char * debugText = "debugText";
  logger->DisplayDebugText(debugText);

  const char * tag = "tag";
  logger->DisplayTag(tag);

  // Check the number of lines written
  unsigned int  numLinesExpected = 7;
  unsigned int  numLinesRead = 0;
  std::ifstream in(logger->GetFileName());
  std::string   line;
  while (std::getline(in, line))
  {
    ++numLinesRead;
  }

  ITK_TEST_EXPECT_EQUAL(numLinesRead, numLinesExpected);


  std::cout << "Test finished." << std::endl;
  return EXIT_SUCCESS;
}
