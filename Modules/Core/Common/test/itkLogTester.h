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
#ifndef itkLogTester_h
#define itkLogTester_h

#include "itkLoggerBase.h"
#include "itkTestingMacros.h"

namespace itk {
namespace Testing {

class LogTester
{
public:
  LogTester(){ this->m_Logger = nullptr; }
  itk::LoggerBase * GetLogger() { return m_Logger; }
  void SetLogger(itk::LoggerBase * logger) { m_Logger = logger; }
  void log() {
    itkLogMacro( PriorityLevelType::DEBUG, "DEBUG message by itkLogMacro\n" );
    itkLogMacro( PriorityLevelType::INFO, "INFO message by itkLogMacro\n" );
    itkLogMacro( PriorityLevelType::WARNING, "WARNING message by itkLogMacro\n" );
    itkLogMacro( PriorityLevelType::CRITICAL, "CRITICAL message by itkLogMacro\n" );
    itkLogMacro( PriorityLevelType::FATAL, "FATAL message by itkLogMacro\n" );
    itkLogMacro( PriorityLevelType::MUSTFLUSH, "MUSTFLUSH message by itkLogMacro\n" );
  }
  static void logStatic(LogTester* tester)
  {
    itkLogMacroStatic( tester, PriorityLevelType::DEBUG, "DEBUG message by itkLogMacroStatic\n" );
    itkLogMacroStatic( tester, PriorityLevelType::INFO, "INFO message by itkLogMacroStatic\n" );
    itkLogMacroStatic( tester, PriorityLevelType::WARNING, "WARNING message by itkLogMacroStatic\n" );
    itkLogMacroStatic( tester, PriorityLevelType::CRITICAL, "CRITICAL message by itkLogMacroStatic\n" );
    itkLogMacroStatic( tester, PriorityLevelType::FATAL, "FATAL message by itkLogMacroStatic\n" );
    itkLogMacroStatic( tester, PriorityLevelType::MUSTFLUSH, "MUSTFLUSH message by itkLogMacroStatic\n" );
  }

private:
  itk::LoggerBase * m_Logger;
};
}
}

#endif // itkLogTester_h
