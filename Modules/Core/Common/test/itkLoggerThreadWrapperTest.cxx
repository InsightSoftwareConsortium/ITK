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

#include "itkLoggerThreadWrapper.h"
#include <iostream>
#include <fstream>
#include "itkStdStreamLogOutput.h"
#include "itkLoggerBase.h"
#include "itkMultiThreaderBase.h"
#include "itkLogTester.h"

/** \class SimpleLogger
 *  \brief Class SimpleLogger is meant to demonstrate how to change the formatting of the LoggerBase mechanism
 *  and how to define a threaded logger that works with this.
 *
 *  \author Hans J. Johnson, The University of Iowa
 *  \ingroup OSSystemObjects LoggingObjects
 */

struct ThreadDataStruct
{
  itk::LoggerBase * logger;
};
using ThreadDataVec = std::vector<ThreadDataStruct>;

class SimpleLogger : public itk::LoggerBase
{
public:
  using Self = SimpleLogger;
  using Superclass = itk::LoggerBase;
  using Pointer = itk::SmartPointer<Self>;
  using ConstPointer = itk::SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(SimpleLogger, Object);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  std::string
  BuildFormattedEntry(PriorityLevelEnum level, std::string const & content) override
  {
    std::string HeaderLevelStart("");
    std::string HeaderLevelStop("");
    switch (level)
    {
      case PriorityLevelEnum::MUSTFLUSH:
        HeaderLevelStart = ("<H1>");
        HeaderLevelStop = ("</H1>");
        break;
      case PriorityLevelEnum::FATAL:
        HeaderLevelStart = ("<H2>");
        HeaderLevelStop = ("</H2>");
        break;
      case PriorityLevelEnum::CRITICAL:
        HeaderLevelStart = ("<H3>");
        HeaderLevelStop = ("</H3>");
        break;
      case PriorityLevelEnum::WARNING:
        HeaderLevelStart = ("<H4>");
        HeaderLevelStop = ("</H4>");
        break;
      case PriorityLevelEnum::INFO:
        HeaderLevelStart = ("<H5>");
        HeaderLevelStop = ("</H5>");
        break;
      case PriorityLevelEnum::DEBUG:
        HeaderLevelStart = ("<H6>");
        HeaderLevelStop = ("</H6>");
        break;
      case PriorityLevelEnum::NOTSET:
        HeaderLevelStart = ("<H7>");
        HeaderLevelStop = ("</H7>");
        break;
    }
    return HeaderLevelStart + content + HeaderLevelStop;
  }

protected:
  /** Constructor */
  SimpleLogger() = default;
  /** Destructor */
  ~SimpleLogger() override = default;
}; // class Logger

ITK_THREAD_RETURN_FUNCTION_CALL_CONVENTION
ThreadedGenerateLogMessages2(void * arg)
{
  const auto * threadInfo = static_cast<itk::MultiThreaderBase::WorkUnitInfo *>(arg);
  if (threadInfo)
  {
    const unsigned int threadId = threadInfo->WorkUnitID;
    std::string        threadPrefix;
    {
      std::ostringstream msg;
      msg << "<Thread " << threadId << "> ";
      threadPrefix = msg.str();
    }

    const ThreadDataVec * dataVec = static_cast<ThreadDataVec *>(threadInfo->UserData);
    if (dataVec)
    {
      const ThreadDataStruct threadData = (*dataVec)[threadId];
      {
        std::ostringstream msg;
        msg << threadPrefix << "unpacked arg\n";
        threadData.logger->Write(itk::LoggerBase::PriorityLevelEnum::INFO, msg.str());
        threadData.logger->Flush();
        msg.str("");
        msg << threadPrefix << "Done logging\n";
        threadData.logger->Write(itk::LoggerBase::PriorityLevelEnum::INFO, msg.str());
        // std::cout << msg.str() << std::endl;
      }
      // do stuff
    }
    else
    {
      std::cerr << "ERROR: UserData was not of type ThreadDataVec*" << std::endl;
      return ITK_THREAD_RETURN_DEFAULT_VALUE;
    }
  }
  else
  {
    std::cerr << "ERROR: arg was not of type itk::MultiThreaderBase::WorkUnitInfo*" << std::endl;
    return ITK_THREAD_RETURN_DEFAULT_VALUE;
  }
  return ITK_THREAD_RETURN_DEFAULT_VALUE;
}

ThreadDataVec
create_threaded_data2(int num_threads, itk::LoggerBase * logger)
{
  ThreadDataVec threadData;
  for (int ii = 0; ii < num_threads; ++ii)
  {
    threadData.push_back(ThreadDataStruct());
    threadData[ii].logger = logger;
  }
  return threadData;
}

int
itkLoggerThreadWrapperTest(int argc, char * argv[])
{
  try
  {
    if (argc < 2)
    {
      std::cout << "Usage: " << itkNameOfTestExecutableMacro(argv) << " logFilename [num threads, default = 10]"
                << std::endl;
      return EXIT_FAILURE;
    }

    int numthreads = 10;
    if (argc > 2)
    {
      numthreads = std::stoi(argv[2]);
    }

    // Create an ITK StdStreamLogOutputs
    itk::StdStreamLogOutput::Pointer coutput = itk::StdStreamLogOutput::New();
    itk::StdStreamLogOutput::Pointer foutput = itk::StdStreamLogOutput::New();
    coutput->SetStream(std::cout);
    std::ofstream fout(argv[1]);
    foutput->SetStream(fout);

    // Create an ITK ThreadLogger
    itk::LoggerThreadWrapper<SimpleLogger>::Pointer logger = itk::LoggerThreadWrapper<SimpleLogger>::New();

    std::cout << "Testing itk::LoggerThreadWrapper" << std::endl;

    // Setting the logger
    logger->SetName("org.itk.threadLogger");
    logger->SetPriorityLevel(itk::LoggerBase::PriorityLevelEnum::INFO);
    logger->SetLevelForFlushing(itk::LoggerBase::PriorityLevelEnum::CRITICAL);

    // Exercising PrintSelf()
    logger->Print(std::cout);

    std::cout << "  Adding console and file stream LogOutputs" << std::endl;
    logger->AddLogOutput(coutput);
    logger->AddLogOutput(foutput);

    // Printing the logger's member variables
    std::cout << "  Name: " << logger->GetName() << std::endl;
    std::cout << "  Priority Level: " << logger->GetPriorityLevel() << std::endl;
    std::cout << "  Level For Flushing: " << logger->GetLevelForFlushing() << std::endl;
    // Print logger itself
    std::cout << logger << std::endl;

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
    logger->Write(itk::LoggerBase::PriorityLevelEnum::DEBUG, "This is the DEBUG message.\n");
    std::cout << "  Message #1" << std::endl;
    logger->Write(itk::LoggerBase::PriorityLevelEnum::INFO, "This is the INFO message.\n");
    logger->Write(itk::LoggerBase::PriorityLevelEnum::WARNING, "This is the WARNING message.\n");
    std::cout << "  Message #2" << std::endl;
    logger->Write(itk::LoggerBase::PriorityLevelEnum::CRITICAL, "This is the CRITICAL message.\n");
    logger->Write(itk::LoggerBase::PriorityLevelEnum::FATAL, "This is the FATAL message.\n");
    logger->Write(itk::LoggerBase::PriorityLevelEnum::MUSTFLUSH, "This is the MUSTFLUSH message.\n");
    std::cout << "  Message #3" << std::endl;
    logger->Flush();
    std::cout << "  Flushing by the ThreadLogger is synchronized." << std::endl;

    std::cout << "Beginning multi-threaded portion of test." << std::endl;
    ThreadDataVec                   threadData = create_threaded_data2(numthreads, logger);
    itk::MultiThreaderBase::Pointer threader = itk::MultiThreaderBase::New();
    itk::MultiThreaderBase::SetGlobalMaximumNumberOfThreads(numthreads + 10);
    threader->SetNumberOfWorkUnits(numthreads);
    threader->SetSingleMethod(ThreadedGenerateLogMessages2, &threadData);
    threader->SingleMethodExecute();
    logger->Flush();
    std::cout << "Ended multi-threaded portion of test." << std::endl;

    std::cout << "Testing SetDelay method" << std::endl;
    logger->SetDelay(1);
    logger->Write(itk::LoggerBase::PriorityLevelEnum::DEBUG, "DEBUG message to tests SetDelay.\n");
    logger->Flush();
    std::cout << "Ended multi-threaded portion of test." << std::endl;

    //
    //  Testing the internal thread
    //
    itk::StdStreamLogOutput::Pointer coutput2 = itk::StdStreamLogOutput::New();
    coutput2->SetStream(std::cout);

    itk::LoggerThreadWrapper<SimpleLogger>::Pointer logger2 = itk::LoggerThreadWrapper<SimpleLogger>::New();

    std::cout << "Testing itk::LoggerThreadWrapper" << std::endl;

    logger2->SetName("org.itk.threadLogger");
    logger2->SetPriorityLevel(itk::LoggerBase::PriorityLevelEnum::INFO);
    logger2->SetLevelForFlushing(itk::LoggerBase::PriorityLevelEnum::CRITICAL);

    logger2->AddLogOutput(coutput2);

    logger2->SetDelay(10);

    logger2->Write(itk::LoggerBase::PriorityLevelEnum::DEBUG, "This is the DEBUG message 1.\n");
    itksys::SystemTools::Delay(1000);
    logger2->Write(itk::LoggerBase::PriorityLevelEnum::DEBUG, "This is the DEBUG message 2.\n");
    itksys::SystemTools::Delay(1000);
    logger2->Write(itk::LoggerBase::PriorityLevelEnum::DEBUG, "This is the DEBUG message 3.\n");
    itksys::SystemTools::Delay(1000);
    logger2->Write(itk::LoggerBase::PriorityLevelEnum::DEBUG, "This is the DEBUG message 4.\n");
    itksys::SystemTools::Delay(1000);
    logger2->Write(itk::LoggerBase::PriorityLevelEnum::DEBUG, "This is the DEBUG message 5.\n");
    itksys::SystemTools::Delay(1000);
    logger2->Write(itk::LoggerBase::PriorityLevelEnum::DEBUG, "This is the DEBUG message 6.\n");

    logger2->SetDelay(1000);
    logger2->SetPriorityLevel(itk::LoggerBase::PriorityLevelEnum::INFO);
    logger2->SetLevelForFlushing(itk::LoggerBase::PriorityLevelEnum::CRITICAL);
    logger2->Flush();
  }
  catch (...)
  {
    std::cerr << "Exception catched !!" << std::endl;
    return EXIT_FAILURE;
  }

  std::cout << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
