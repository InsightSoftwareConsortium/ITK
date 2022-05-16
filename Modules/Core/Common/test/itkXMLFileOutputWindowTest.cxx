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


#include "itkXMLFileOutputWindow.h"
#include "itkTestingMacros.h"
#include <fstream>
#include <cstdio>
#include "itksys/SystemTools.hxx"
#ifdef _WIN32
#  include <direct.h>
#  define cwd _getcwd
static const std::string OS_SEP{ "\\" };
#else
#  include "unistd.h"
#  define cwd getcwd
static const std::string OS_SEP{ "/" };
#endif


static bool
DoTestXMLFileOutputWindow(std::string currentLoggerFilename, const unsigned int requiredLinesInFile)
{
  if (currentLoggerFilename.empty())
  {
    const std::string fileBaseName = []() -> std::string {
      // If no input filename is provided, remove the default file to avoid counting existing lines when
      // contents are appended.
      itk::XMLFileOutputWindow::Pointer logger = itk::XMLFileOutputWindow::New();
      // In order to initialize the filename, some text needs to be written first
      const char * regularText = "text";
      logger->DisplayText(regularText);

      // Get the filename
      return std::string{ logger->GetFileName() };
    }();
    const std::string cwd = itksys::SystemTools::GetCurrentWorkingDirectory();
    currentLoggerFilename = cwd + OS_SEP + fileBaseName;
    std::remove(currentLoggerFilename.c_str());
  }


  itk::XMLFileOutputWindow::Pointer logger = itk::XMLFileOutputWindow::New();

  ITK_EXERCISE_BASIC_OBJECT_METHODS(logger, XMLFileOutputWindow, FileOutputWindow);

  logger->FlushOn();
  logger->AppendOn();

  logger->SetInstance(logger);
  logger->SetFileName(currentLoggerFilename);


  // Check special cases
  const char * const text = nullptr;
  logger->DisplayText(text);

  const char * const specialCharText = "text&\"'<>";
  logger->DisplayText(specialCharText);

  // Regular cases
  const char * const regularText = "text";
  logger->DisplayText(regularText);

  const char * const errorText = "errorText";
  logger->DisplayErrorText(errorText);

  const char * const warningText = "warningText";
  logger->DisplayWarningText(warningText);

  const char * const genericOutputText = "genericOutputText";
  logger->DisplayGenericOutputText(genericOutputText);

  const char * const debugText = "debugText";
  logger->DisplayDebugText(debugText);

  const char * const tag = "tag";
  logger->DisplayTag(tag);

  // Check the number of lines written
  unsigned int  numLinesRead = 0;
  std::ifstream in(logger->GetFileName());
  std::string   line;
  while (std::getline(in, line))
  {
    ++numLinesRead;
  }

  std::cout << "File used: " << currentLoggerFilename << std::endl;
  ITK_TEST_EXPECT_EQUAL(numLinesRead, requiredLinesInFile);

  std::cout << "Test finished: " << numLinesRead << " (read) == " << requiredLinesInFile << "(required)" << std::endl;
  return numLinesRead == requiredLinesInFile;
}


int
itkXMLFileOutputWindowTest(int argc, char * argv[])
{
  if (argc != 2)
  {
    std::cerr << "Missing parameters." << std::endl;
    std::cerr << "Usage: " << itkNameOfTestExecutableMacro(argv) << " filename" << std::endl;
    return EXIT_FAILURE;
  }

  // First test with no filename given, to test autogenerating the filename
  constexpr unsigned int numLinesExpected = 7;
  bool                   status = DoTestXMLFileOutputWindow("", numLinesExpected);
  const std::string      test1Message{ status ? "TEST ONE PASSED" : "TEST ONE FAILED" };
  std::cout << test1Message << "\n\n" << std::endl;
  // Second test writing a new file with filename given
  std::remove(argv[1]);
  status &= DoTestXMLFileOutputWindow(argv[1], numLinesExpected);
  const std::string test2Message{ status ? "TEST TWO PASSED" : "TEST TWO FAILED" };
  std::cout << test2Message << "\n\n" << std::endl;
  // Third test with appending to filename given
  status &= DoTestXMLFileOutputWindow(argv[1], numLinesExpected * 2);
  const std::string test3Message{ status ? "TEST THREE PASSED" : "TEST THREE FAILED" };
  std::cout << test3Message << "\n\n" << std::endl;

  return (status == true) ? EXIT_SUCCESS : EXIT_FAILURE;
}
