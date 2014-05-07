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
#include "itkThreadLogger.h"


struct ThreadDataStruct
{
  itk::LoggerBase* logger;
};
typedef std::vector<ThreadDataStruct> ThreadDataVec;

class LogTester
{
public:
  LogTester(){ this->m_Logger = ITK_NULLPTR; }
  itk::Logger* GetLogger() { return m_Logger; }
  void SetLogger(itk::Logger* logger) { m_Logger = logger; }
  void log() {
    itkLogMacro( DEBUG, "DEBUG message by itkLogMacro\n" );
    itkLogMacro( INFO, "INFO message by itkLogMacro\n" );
    itkLogMacro( WARNING, "WARNING message by itkLogMacro\n" );
    itkLogMacro( CRITICAL, "CRITICAL message by itkLogMacro\n" );
    itkLogMacro( FATAL, "FATAL message by itkLogMacro\n" );
    itkLogMacro( MUSTFLUSH, "MUSTFLUSH message by itkLogMacro\n" );
  }
  static void logStatic(LogTester* tester)
  {
    itkLogMacroStatic( tester, DEBUG, "DEBUG message by itkLogMacroStatic\n" );
    itkLogMacroStatic( tester, INFO, "INFO message by itkLogMacroStatic\n" );
    itkLogMacroStatic( tester, WARNING, "WARNING message by itkLogMacroStatic\n" );
    itkLogMacroStatic( tester, CRITICAL, "CRITICAL message by itkLogMacroStatic\n" );
    itkLogMacroStatic( tester, FATAL, "FATAL message by itkLogMacroStatic\n" );
    itkLogMacroStatic( tester, MUSTFLUSH, "MUSTFLUSH message by itkLogMacroStatic\n" );
  }

private:
  itk::Logger* m_Logger;
};

ITK_THREAD_RETURN_TYPE ThreadedGenerateLogMessages(void* arg)
{
  const itk::MultiThreader::ThreadInfoStruct* threadInfo =
                   static_cast<itk::MultiThreader::ThreadInfoStruct*>(arg);
  if (threadInfo)
  {
    const unsigned int threadId = threadInfo->ThreadID;
    std::string threadPrefix;
    {
      std::ostringstream msg;
      msg << "<Thread " << threadId << "> ";
      threadPrefix = msg.str();
    }

    const ThreadDataVec* dataVec = static_cast<ThreadDataVec*>(threadInfo->UserData);
    if (dataVec)
    {
      const ThreadDataStruct threadData = (*dataVec)[threadId];
      {
        std::ostringstream msg;
        msg << threadPrefix << "unpacked arg\n";
        threadData.logger->Write(itk::LoggerBase::INFO, msg.str());
        threadData.logger->Flush();
        msg.str("");
        msg << threadPrefix << "Done logging\n";
        threadData.logger->Write(itk::LoggerBase::INFO, msg.str());
        //std::cout << msg.str() << std::endl;
      }
     // do stuff
    } else {
      std::cerr << "ERROR: UserData was not of type ThreadDataVec*" << std::endl;
      return ITK_THREAD_RETURN_VALUE;
    }
  } else {
    std::cerr << "ERROR: arg was not of type itk::MultiThreader::ThreadInfoStruct*" << std::endl;
    return ITK_THREAD_RETURN_VALUE;
  }
  return ITK_THREAD_RETURN_VALUE;
}

ThreadDataVec create_threaded_data(int num_threads, itk::LoggerBase* logger)
{
  ThreadDataVec threadData;
  for (int ii = 0; ii < num_threads; ++ii)
  {
    threadData.push_back(ThreadDataStruct());
    threadData[ii].logger = logger;
  }
  return threadData;
}

int itkThreadLoggerTest( int argc, char * argv[] )
{
  try
    {
    if (argc < 2)
      {
      std::cout << "Usage: " << argv[0] << " logFilename [num threads, default = 10]" << std::endl;
      return EXIT_FAILURE;
      }

    int numthreads = 10;
    if (argc > 2)
    {
      numthreads = atoi(argv[2]);
    }

    // Create an ITK StdStreamLogOutputs
    itk::StdStreamLogOutput::Pointer coutput = itk::StdStreamLogOutput::New();
    itk::StdStreamLogOutput::Pointer foutput = itk::StdStreamLogOutput::New();
    coutput->SetStream(std::cout);
    std::ofstream fout(argv[1]);
    foutput->SetStream(fout);

    // Create an ITK ThreadLogger
    itk::ThreadLogger::Pointer logger = itk::ThreadLogger::New();

    std::cout << "Testing itk::ThreadLogger" << std::endl;

    // Setting the logger
    logger->SetName("org.itk.threadLogger");
    logger->SetPriorityLevel(itk::LoggerBase::INFO);
    logger->SetLevelForFlushing(itk::LoggerBase::CRITICAL);

    std::cout << "  Adding console and file stream LogOutputs" << std::endl;
    logger->AddLogOutput(coutput);
    logger->AddLogOutput(foutput);

    // Printing the logger's member variables
    std::cout << "  Name: " << logger->GetName() << std::endl;
    std::cout << "  Priority Level: " << logger->GetPriorityLevel() << std::endl;
    std::cout << "  Level For Flushing: " << logger->GetLevelForFlushing() << std::endl;

    // Logging by the itkLogMacro from a class with itk::ThreadLogger
    LogTester tester;
    tester.SetLogger(logger);
    tester.log();
    // Logging by the itkLogMacroStatic from a class with itk::ThreadLogger
    LogTester::logStatic(&tester);

    std::cout << "  The printed order of 'Messages ##' below might not be predictable because of multi-threaded logging" << std::endl;
    std::cout << "  But the logged messages will be in order." << std::endl;
    std::cout << "  Each line is an atom for synchronization." << std::endl;
    // Writing by the logger
    logger->Write(itk::LoggerBase::DEBUG, "This is the DEBUG message.\n");
    std::cout << "  Message #1" << std::endl;
    logger->Write(itk::LoggerBase::INFO, "This is the INFO message.\n");
    logger->Write(itk::LoggerBase::WARNING, "This is the WARNING message.\n");
    std::cout << "  Message #2" << std::endl;
    logger->Write(itk::LoggerBase::CRITICAL, "This is the CRITICAL message.\n");
    logger->Write(itk::LoggerBase::FATAL, "This is the FATAL message.\n");
    logger->Write(itk::LoggerBase::MUSTFLUSH, "This is the MUSTFLUSH message.\n");
    std::cout << "  Message #3" << std::endl;
    logger->Flush();
    std::cout << "  Flushing by the ThreadLogger is synchronized." << std::endl;

    std::cout << "Beginning multi-threaded portion of test." << std::endl;
    ThreadDataVec threadData = create_threaded_data(numthreads, logger);
    itk::MultiThreader::Pointer threader = itk::MultiThreader::New();
    threader->SetGlobalMaximumNumberOfThreads(numthreads + 10);
    threader->SetNumberOfThreads(numthreads);
    threader->SetSingleMethod(ThreadedGenerateLogMessages, &threadData);
    threader->SingleMethodExecute();
    logger->Flush();
    std::cout << "Ended multi-threaded portion of test." << std::endl;

    }
  catch(...)
    {
    std::cerr << "Exception catched !!" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
