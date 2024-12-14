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
#include "itkLoggerManager.h"
#include "itkLogTester.h"
#include "itkTestingMacros.h"


int
itkLoggerManagerTest(int argc, char * argv[])
{
  try
  {
    if (argc < 2)
    {
      std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << " logFilename" << '\n';
      return EXIT_FAILURE;
    }

    // Create an ITK StdStreamLogOutputs
    const itk::StdStreamLogOutput::Pointer coutput = itk::StdStreamLogOutput::New();
    const itk::StdStreamLogOutput::Pointer foutput = itk::StdStreamLogOutput::New();
    coutput->SetStream(std::cout);
    std::ofstream fout(argv[1]);
    foutput->SetStream(fout);

    // Create an ITK Loggers using itk::LoggerManager
    const itk::LoggerManager::Pointer manager = itk::LoggerManager::New();

    ITK_EXERCISE_BASIC_OBJECT_METHODS(manager, LoggerManager, Object);


    const itk::Logger::Pointer logger = manager->CreateLogger("org.itk.logTester.logger",
                                                              itk::LoggerBase::PriorityLevelEnum::DEBUG,
                                                              itk::LoggerBase::PriorityLevelEnum::CRITICAL);

    const itk::ThreadLogger::Pointer t_logger =
      manager->CreateThreadLogger("org.itk.ThreadLogger",
                                  itk::LoggerBase::PriorityLevelEnum::WARNING,
                                  itk::LoggerBase::PriorityLevelEnum::CRITICAL);

    std::cout << "Testing itk::LoggerManager" << '\n';

    std::cout << "  Adding console and file stream LogOutputs" << '\n';
    manager->AddLogOutput(coutput);
    t_logger->AddLogOutput(foutput);

    // Logging by the itkLogMacro from a class with itk::ThreadLogger
    itk::Testing::LogTester tester;
    tester.SetLogger(logger);
    tester.log();
    // Logging by the itkLogMacroStatic from a class with itk::ThreadLogger
    itk::Testing::LogTester::logStatic(&tester);

    std::cout << "  The printed order of 'Messages ##' below might not be predictable because of multi-threaded logging"
              << '\n';
    std::cout << "  But the logged messages will be in order." << '\n';
    std::cout << "  Each line is an atom for synchronization." << '\n';
    // Writing by the logger
    manager->Write(itk::LoggerBase::PriorityLevelEnum::DEBUG, "This is the DEBUG message.\n");
    std::cout << "  Message #1" << '\n';
    manager->Write(itk::LoggerBase::PriorityLevelEnum::INFO, "This is the INFO message.\n");
    manager->Write(itk::LoggerBase::PriorityLevelEnum::WARNING, "This is the WARNING message.\n");
    std::cout << "  Message #2" << '\n';
    manager->Write(itk::LoggerBase::PriorityLevelEnum::CRITICAL, "This is the CRITICAL message.\n");
    manager->Write(itk::Logger::PriorityLevelEnum::FATAL, "This is the FATAL message.\n");
    manager->Write(itk::LoggerBase::PriorityLevelEnum::MUSTFLUSH, "This is the MUSTFLUSH message.\n");
    std::cout << "  Message #3" << '\n';
    itk::Logger * pLogger = manager->GetLogger("org.itk.logTester.logger");
    if (pLogger == nullptr)
    {
      throw "LoggerManager::GetLogger() failed";
    }
    pLogger->Write(itk::LoggerBase::PriorityLevelEnum::INFO,
                   "This is the message from the logger got from a LoggerManager");
    if (manager->GetLogger("abc") != nullptr)
    {
      throw "LoggerManager::GetLogger() must return nullptr";
    }
    manager->Flush();
  }
  catch (const char * errmsg)
  {
    std::cerr << "Exception caught! : " << errmsg << '\n';
    return EXIT_FAILURE;
  }

  std::cout << "[PASSED]" << '\n';
  return EXIT_SUCCESS;
}
