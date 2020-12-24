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

class ITKCommon_EXPORT LoggerManager : public Object
{
public:
  using Self = LoggerManager;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(LoggerManager, Object);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  using PriorityLevelEnum = Logger::PriorityLevelEnum;

  using OutputType = Logger::OutputType;

  using LoggerPointer = Logger::Pointer;
  using ThreadLoggerPointer = ThreadLogger::Pointer;

  using NameType = std::string;

  /** create a logger and add it into LoggerManager */
  LoggerPointer
  CreateLogger(const NameType &  name,
               PriorityLevelEnum level,
               PriorityLevelEnum levelForFlushing = LoggerBase::PriorityLevelEnum::MUSTFLUSH);

  /** create a thread logger and add it into LoggerManager */
  ThreadLoggerPointer
  CreateThreadLogger(const NameType &  name,
                     PriorityLevelEnum level,
                     PriorityLevelEnum levelForFlushing = LoggerBase::PriorityLevelEnum::MUSTFLUSH);

  /** Registers a logger */
  void
  AddLogger(const NameType & name, Logger * logger);

  Logger *
  GetLogger(const NameType & name);

  void
  SetPriorityLevel(PriorityLevelEnum level);

  void
  SetLevelForFlushing(PriorityLevelEnum level);

  void
  AddLogOutput(OutputType * output);

  void
  Write(PriorityLevelEnum level, std::string const & content);

  void
  Flush();

protected:
  /** Constructor */
  LoggerManager() = default;

  /** Destructor */
  ~LoggerManager() override = default;

  /** Print contents of a LoggerManager */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

private:
  using ContainerType = std::map<NameType, LoggerPointer>;

  ContainerType m_LoggerSet;
}; // class Logger
} // namespace itk

#endif // itkLoggerManager_h
