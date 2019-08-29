/*=========================================================================
 *
 *  Copyright Insight Software Consortium
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

#include <iostream>
#include <fstream>
#include "itkStdStreamLogOutput.h"
#include "itkLoggerManager.h"
#include "itkLogTester.h"


int
itkLoggerManagerTest(int argc, char * argv[])
{
  try
  {
    if (argc < 2)
    {
      std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << " logFilename" << std::endl;
      return EXIT_FAILURE;
    }

    // Create an ITK StdStreamLogOutputs
    itk::StdStreamLogOutput::Pointer coutput = itk::StdStreamLogOutput::New();
    itk::StdStreamLogOutput::Pointer foutput = itk::StdStreamLogOutput::New();
    coutput->SetStream(std::cout);
    std::ofstream fout(argv[1]);
    foutput->SetStream(fout);

    // Create an ITK Loggers using itk::LoggerManager
    itk::LoggerManager::Pointer manager = itk::LoggerManager::New();

    itk::Logger::Pointer logger = manager->CreateLogger("org.itk.logTester.logger",
                                                        itk::LoggerBase::PriorityLevelType::DEBUG,
                                                        itk::LoggerBase::PriorityLevelType::CRITICAL);

    itk::ThreadLogger::Pointer t_logger = manager->CreateThreadLogger("org.itk.ThreadLogger",
                                                                      itk::LoggerBase::PriorityLevelType::WARNING,
                                                                      itk::LoggerBase::PriorityLevelType::CRITICAL);

    std::cout << "Testing itk::LoggerManager" << std::endl;

    std::cout << "  Adding console and file stream LogOutputs" << std::endl;
    manager->AddLogOutput(coutput);
    t_logger->AddLogOutput(foutput);

    // Logging by the itkLogMacro from a class with itk::ThreadLogger
    itk::Testing::LogTester tester;
    tester.SetLogger(logger);
    tester.log();
    // Logging by the itkLogMacroStatic from a class with itk::ThreadLogger
    itk::Testing::LogTester::logStatic(&tester);

    std::cout << "  The printed order of 'Messages ##' below might not be predictable because of multi-threaded logging"
              << std::endl;
    std::cout << "  But the logged messages will be in order." << std::endl;
    std::cout << "  Each line is an atom for synchronization." << std::endl;
    // Writing by the logger
    manager->Write(itk::LoggerBase::PriorityLevelType::DEBUG, "This is the DEBUG message.\n");
    std::cout << "  Message #1" << std::endl;
    manager->Write(itk::LoggerBase::PriorityLevelType::INFO, "This is the INFO message.\n");
    manager->Write(itk::LoggerBase::PriorityLevelType::WARNING, "This is the WARNING message.\n");
    std::cout << "  Message #2" << std::endl;
    manager->Write(itk::LoggerBase::PriorityLevelType::CRITICAL, "This is the CRITICAL message.\n");
    manager->Write(itk::Logger::PriorityLevelType::FATAL, "This is the FATAL message.\n");
    manager->Write(itk::LoggerBase::PriorityLevelType::MUSTFLUSH, "This is the MUSTFLUSH message.\n");
    std::cout << "  Message #3" << std::endl;
    itk::Logger * pLogger;
    pLogger = manager->GetLogger("org.itk.logTester.logger");
    if (pLogger == nullptr)
    {
      throw "LoggerManager::GetLogger() failed";
    }
    pLogger->Write(itk::LoggerBase::PriorityLevelType::INFO,
                   "This is the message from the logger got from a LoggerManager");
    if (manager->GetLogger("abc") != nullptr)
    {
      throw "LoggerManager::GetLogger() must return nullptr";
    }
    manager->Flush();
  }
  catch (const char * errmsg)
  {
    std::cerr << "Exception caught! : " << errmsg << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
