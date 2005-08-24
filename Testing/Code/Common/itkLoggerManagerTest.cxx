/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLoggerManagerTest.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#if defined(_MSC_VER)
   //Warning about: identifier was truncated to '255' characters in the debug information (MVC6.0 Debug)
#pragma warning( disable : 4786 )
#endif

#include <iostream>
#include <fstream>
#include "itkStdStreamLogOutput.h"
#include "itkThreadLogger.h"
#include "itkLoggerManager.h"



class LogTester
{
public:
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



int itkLoggerManagerTest( int argc, char *argv [] )
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

    // Create an ITK Loggers using itk::LoggerManager
    itk::LoggerManager::Pointer manager = itk::LoggerManager::New();

    itk::Logger::Pointer logger = manager->CreateLogger( "org.itk.logTester.logger", 
      itk::Logger::DEBUG, itk::Logger::CRITICAL );

    itk::ThreadLogger::Pointer t_logger = manager->CreateThreadLogger( "org.itk.ThreadLogger",
      itk::Logger::WARNING, itk::Logger::CRITICAL );

    std::cout << "Testing itk::LoggerManager" << std::endl;

    std::cout << "  Adding console and file stream LogOutputs" << std::endl;
    manager->AddLogOutput(coutput);
    t_logger->AddLogOutput(foutput);

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
    manager->Write(itk::Logger::DEBUG, "This is the DEBUG message.\n");
    std::cout << "  Message #1" << std::endl;
    manager->Write(itk::Logger::INFO, "This is the INFO message.\n");
    manager->Write(itk::Logger::WARNING, "This is the WARNING message.\n");
    std::cout << "  Message #2" << std::endl;
    manager->Write(itk::Logger::CRITICAL, "This is the CRITICAL message.\n");
    manager->Write(itk::Logger::FATAL, "This is the FATAL message.\n");
    manager->Write(itk::Logger::MUSTFLUSH, "This is the MUSTFLUSH message.\n");
    std::cout << "  Message #3" << std::endl;
    itk::Logger* pLogger;
    pLogger = manager->GetLogger("org.itk.logTester.logger");
    if( pLogger == NULL )
    {
      throw "LoggerManager::GetLogger() failed";
    }
    pLogger->Write(itk::Logger::INFO, "This is the message from the logger got from a LoggerManager");
    if( manager->GetLogger("abc") != NULL )
    {
      throw "LoggerManager::GetLogger() must return NULL";
    }
    manager->Flush();
    }
  catch(const char * errmsg)
    {
    std::cerr << "Exception catched !! : " << errmsg << std::endl;
    return EXIT_FAILURE;
    }

  std::cout << "[PASSED]" << std::endl;
  return EXIT_SUCCESS;
}


