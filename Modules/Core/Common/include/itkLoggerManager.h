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
#ifndef itkLoggerManager_h
#define itkLoggerManager_h

#include "itkObject.h"
#include "itkObjectFactory.h"
#include "itkThreadLogger.h"

#include <string>
#include <map>

namespace itk
{
/** \class LoggerManager
 *  \brief Used for centrally managing loggers.
 *
 * \author Hee-Su Kim, Compute Science Dept. Kyungpook National University,
 *                     ISIS Center, Georgetown University.
 *
 *
 * \ingroup OSSystemObjects LoggingObjects
 *
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT LoggerManager:public Object
{
public:

  typedef LoggerManager              Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(LoggerManager, Object);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  typedef Logger::PriorityLevelType PriorityLevelType;

  typedef Logger::OutputType OutputType;

  typedef Logger::Pointer       LoggerPointer;
  typedef ThreadLogger::Pointer ThreadLoggerPointer;

  typedef std::string NameType;

  /** create a logger and add it into LoggerManager */
  LoggerPointer CreateLogger(
    const NameType & name,
    PriorityLevelType level,
    PriorityLevelType levelForFlushing = LoggerBase::MUSTFLUSH);

  /** create a thread logger and add it into LoggerManager */
  ThreadLoggerPointer CreateThreadLogger(
    const NameType & name,
    PriorityLevelType level,
    PriorityLevelType levelForFlushing = LoggerBase::MUSTFLUSH);

  /** Registers a logger */
  void AddLogger(const NameType & name, Logger *logger);

  Logger * GetLogger(const NameType & name);

  void SetPriorityLevel(PriorityLevelType level);

  void SetLevelForFlushing(PriorityLevelType level);

  void AddLogOutput(OutputType *output);

  void Write(PriorityLevelType level, std::string const & content);

  void Flush();

protected:

  /** Constructor */
  LoggerManager() {}

  /** Destructor */
  virtual ~LoggerManager() ITK_OVERRIDE {}

  /** Print contents of a LoggerManager */
  virtual void PrintSelf(std::ostream & s, Indent indent) const ITK_OVERRIDE;

private:

  typedef std::map< NameType, LoggerPointer > ContainerType;

  ContainerType m_LoggerSet;
};  // class Logger
} // namespace itk

#endif  // itkLoggerManager_h
