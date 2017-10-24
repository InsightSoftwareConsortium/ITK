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

#include "itkLoggerThreadWrapper.h"
#include <iostream>
#include <fstream>
#include "itkStdStreamLogOutput.h"
#include "itkLoggerBase.h"

/** \class SimpleLogger
 *  \brief Class SimpleLogger is meant to demonstrate how to change the formatting of the LoggerBase mechanism
 *  and how to define a threaded logger that works with this.
 *
 *  \author Hans J. Johnson, The University of Iowa
 *  \ingroup OSSystemObjects LoggingObjects
 */

struct ThreadDataStruct
{
  itk::LoggerBase* logger;
};
typedef std::vector<ThreadDataStruct> ThreadDataVec;

class SimpleLogger : public itk::LoggerBase
{
public:
  typedef SimpleLogger                  Self;
  typedef itk::LoggerBase               Superclass;
  typedef itk::SmartPointer<Self>       Pointer;
  typedef itk::SmartPointer<const Self> ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro( SimpleLogger, Object );

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro( Self );

  virtual std::string BuildFormattedEntry(PriorityLevelType level, std::string const & content) ITK_OVERRIDE
    {
    std::string HeaderLevelStart("");
    std::string HeaderLevelStop("");
    switch(level)
      {
    case MUSTFLUSH:
      HeaderLevelStart=("<H1>");
      HeaderLevelStop=("</H1>");
      break;
    case FATAL:
      HeaderLevelStart=("<H2>");
      HeaderLevelStop=("</H2>");
      break;
    case CRITICAL:
      HeaderLevelStart=("<H3>");
      HeaderLevelStop=("</H3>");
      break;
    case WARNING:
      HeaderLevelStart=("<H4>");
      HeaderLevelStop=("</H4>");
      break;
    case INFO:
      HeaderLevelStart=("<H5>");
      HeaderLevelStop=("</H5>");
      break;
    case DEBUG:
      HeaderLevelStart=("<H6>");
      HeaderLevelStop=("</H6>");
      break;
    case NOTSET:
      HeaderLevelStart=("<H7>");
      HeaderLevelStop=("</H7>");
      break;
      }
    return HeaderLevelStart + content + HeaderLevelStop;
    }

protected:
  /** Constructor */
  SimpleLogger() {};
  /** Destructor */
  virtual ~SimpleLogger() ITK_OVERRIDE {};
};  // class Logger

class LogTester
{
public:
  LogTester(){ this->m_Logger = ITK_NULLPTR; }
  itk::LoggerBase* GetLogger() { return m_Logger; }
  void SetLogger(itk::LoggerBase* logger) { m_Logger = logger; }
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
  itk::LoggerBase* m_Logger;
};

ITK_THREAD_RETURN_TYPE ThreadedGenerateLogMessages2(void* arg)
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

ThreadDataVec create_threaded_data2(int num_threads, itk::LoggerBase* logger)
{
  ThreadDataVec threadData;
  for (int ii = 0; ii < num_threads; ++ii)
    {
    threadData.push_back(ThreadDataStruct());
    threadData[ii].logger = logger;
    }
  return threadData;
}

int itkLoggerThreadWrapperTest( int argc, char * argv[] )
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
    itk::LoggerThreadWrapper<SimpleLogger>::Pointer logger = itk::LoggerThreadWrapper<SimpleLogger>::New();

    std::cout << "Testing itk::LoggerThreadWrapper" << std::endl;

    // Setting the logger
    logger->SetName("org.itk.threadLogger");
    logger->SetPriorityLevel(itk::LoggerBase::INFO);
    logger->SetLevelForFlushing(itk::LoggerBase::CRITICAL);

    // Exercising PrintSelf()
    logger->Print(std::cout);

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
    ThreadDataVec threadData = create_threaded_data2(numthreads, logger);
    itk::MultiThreader::Pointer threader = itk::MultiThreader::New();
    threader->SetGlobalMaximumNumberOfThreads(numthreads + 10);
    threader->SetNumberOfThreads(numthreads);
    threader->SetSingleMethod(ThreadedGenerateLogMessages2, &threadData);
    threader->SingleMethodExecute();
    logger->Flush();
    std::cout << "Ended multi-threaded portion of test." << std::endl;

    std::cout << "Testing SetDelay method" << std::endl;
    logger->SetDelay(1);
    logger->Write(itk::LoggerBase::DEBUG, "DEBUG message to tests SetDelay.\n");
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
    logger2->SetPriorityLevel(itk::LoggerBase::INFO);
    logger2->SetLevelForFlushing(itk::LoggerBase::CRITICAL);

    logger2->AddLogOutput(coutput2);

    logger2->SetDelay(10);

    logger2->Write(itk::LoggerBase::DEBUG, "This is the DEBUG message 1.\n");
    itksys::SystemTools::Delay(1000);
    logger2->Write(itk::LoggerBase::DEBUG, "This is the DEBUG message 2.\n");
    itksys::SystemTools::Delay(1000);
    logger2->Write(itk::LoggerBase::DEBUG, "This is the DEBUG message 3.\n");
    itksys::SystemTools::Delay(1000);
    logger2->Write(itk::LoggerBase::DEBUG, "This is the DEBUG message 4.\n");
    itksys::SystemTools::Delay(1000);
    logger2->Write(itk::LoggerBase::DEBUG, "This is the DEBUG message 5.\n");
    itksys::SystemTools::Delay(1000);
    logger2->Write(itk::LoggerBase::DEBUG, "This is the DEBUG message 6.\n");

    logger2->SetDelay(1000);
    logger2->SetPriorityLevel(itk::LoggerBase::INFO);
    logger2->SetLevelForFlushing(itk::LoggerBase::CRITICAL);
    logger2->Flush();
    }
  catch(...)
    {
    std::cerr << "Exception catched !!" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
