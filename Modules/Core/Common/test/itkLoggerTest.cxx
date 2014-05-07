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
#include "itkLogger.h"

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
    itkLogMacroStatic(tester, DEBUG, "DEBUG message by itkLogMacroStatic\n" );
    itkLogMacroStatic(tester, INFO, "INFO message by itkLogMacroStatic\n" );
    itkLogMacroStatic(tester, WARNING, "WARNING message by itkLogMacroStatic\n" );
    itkLogMacroStatic(tester, CRITICAL, "CRITICAL message by itkLogMacroStatic\n" );
    itkLogMacroStatic(tester, FATAL, "FATAL message by itkLogMacroStatic\n" );
    itkLogMacroStatic(tester, MUSTFLUSH, "MUSTFLUSH message by itkLogMacroStatic\n" );
  }

private:
  itk::Logger* m_Logger;
};

int itkLoggerTest( int argc, char *argv [] )
{
  try
    {
    if (argc < 2)
      {
      std::cout << "Usage: " << argv[0] << " logFilename" << std::endl;
      return EXIT_FAILURE;
      }

    // Create an ITK StdStreamLogOutputs
    itk::StdStreamLogOutput::Pointer coutput = itk::StdStreamLogOutput::New();
    itk::StdStreamLogOutput::Pointer foutput = itk::StdStreamLogOutput::New();
    coutput->SetStream(std::cout);
    std::ofstream fout(argv[1]);
    foutput->SetStream(fout);

    // Create an ITK Logger
    itk::Logger::Pointer logger = itk::Logger::New();

    std::cout << "Testing itk::Logger" << std::endl;

    // Setting the logger
    logger->SetName("org.itk.rootLogger");
    logger->SetPriorityLevel(itk::LoggerBase::INFO);
    logger->SetLevelForFlushing(itk::LoggerBase::CRITICAL);

    std::cout << "  Adding console and file stream LogOutputs" << std::endl;
    logger->AddLogOutput(coutput);
    logger->AddLogOutput(foutput);

    // Printing the logger's member variables
    std::cout << "  Name: " << logger->GetName() << std::endl;
    std::cout << "  Priority Level: " << logger->GetPriorityLevel() << std::endl;
    std::cout << "  Level For Flushing: " << logger->GetLevelForFlushing() << std::endl;

    // Logging by the itkLogMacro from a class with itk::Logger
    std::cout << "  Logging by the itkLogMacro from a class with itk::Logger" << std::endl;
    LogTester tester;
    tester.SetLogger(logger);
    tester.log();
    // Logging by the itkLogMacroStatic from a class with itk::Logger
    std::cout << "  Logging by the itkLogMacroStatic from a class with itk::Logger" << std::endl;
    LogTester::logStatic(&tester);

    // Writing by the logger
    std::cout << "  Writing by itk::Logger" << std::endl;
    logger->Write(itk::LoggerBase::DEBUG, "This is the DEBUG message.\n");
    logger->Write(itk::LoggerBase::INFO, "This is the INFO message.\n");
    logger->Write(itk::LoggerBase::WARNING, "This is the WARNING message.\n");
    logger->Write(itk::LoggerBase::CRITICAL, "This is the CRITICAL message.\n");
    logger->Write(itk::LoggerBase::FATAL, "This is the FATAL message.\n");
    logger->Write(itk::LoggerBase::MUSTFLUSH, "This is the MUSTFLUSH message.\n");
    logger->Flush();

    itk::LoggerBase::TimeStampFormatType timeStampFormat = itk::LoggerBase::HUMANREADABLE;
    logger->SetTimeStampFormat( timeStampFormat );

    if( logger->GetTimeStampFormat() != timeStampFormat )
      {
      std::cerr << "Error in SetTimeStampFormat()/GetTimeStampFormat()" << std::endl;
      return EXIT_FAILURE;
      }


    std::cout << "  Writing by itk::Logger in Human Readable format" << std::endl;
    logger->Write(itk::LoggerBase::DEBUG, "This is the DEBUG message.\n");
    logger->Write(itk::LoggerBase::INFO, "This is the INFO message.\n");
    logger->Write(itk::LoggerBase::WARNING, "This is the WARNING message.\n");
    logger->Write(itk::LoggerBase::CRITICAL, "This is the CRITICAL message.\n");
    logger->Write(itk::LoggerBase::FATAL, "This is the FATAL message.\n");
    logger->Write(itk::LoggerBase::MUSTFLUSH, "This is the MUSTFLUSH message.\n");
    logger->Flush();


    std::string humanReadableFormat = "%b %d, %Y, %H:%M:%S";
    logger->SetHumanReadableFormat( humanReadableFormat );

    if( logger->GetHumanReadableFormat() != humanReadableFormat )
      {
      std::cerr << "Error in SetHumanReadableFormat()/GetHumanReadableFormat()" << std::endl;
      return EXIT_FAILURE;
      }

    std::cout << "  Writing by itk::Logger in Human Readable style with new format" << std::endl;
    logger->Write(itk::LoggerBase::DEBUG, "This is the DEBUG message.\n");
    logger->Write(itk::LoggerBase::INFO, "This is the INFO message.\n");
    logger->Write(itk::LoggerBase::WARNING, "This is the WARNING message.\n");
    logger->Write(itk::LoggerBase::CRITICAL, "This is the CRITICAL message.\n");
    logger->Write(itk::LoggerBase::FATAL, "This is the FATAL message.\n");
    logger->Write(itk::LoggerBase::MUSTFLUSH, "This is the MUSTFLUSH message.\n");
    logger->Flush();

    }
  catch(...)
    {
    std::cerr << "Exception catched !!" << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}
