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
#ifndef itkThreadLogger_h
#define itkThreadLogger_h

#include "itkLogger.h"
#include <mutex>

#include <string>
#include <queue>
#include <thread>

namespace itk
{
/** \class ThreadLogger
 *  \brief Providing logging service as a separate thread.
 *
 * \author Hee-Su Kim, Compute Science Dept. Kyungpook National University,
 *                     ISIS Center, Georgetown University.
 *
 *  \ingroup OSSystemObjects LoggingObjects
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT ThreadLogger : public Logger
{
public:
  using Self = ThreadLogger;
  using Superclass = Logger;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(ThreadLogger, Logger);

  /** New macro for creation of a Smart Pointer */
  itkNewMacro(Self);

  using OutputType = Logger::OutputType;

  using PriorityLevelEnum = Logger::PriorityLevelEnum;

  using DelayType = unsigned int;

  /** Definition of types of operations for ThreadLogger. */
  typedef enum
  {
    SET_PRIORITY_LEVEL,
    SET_LEVEL_FOR_FLUSHING,
    ADD_LOG_OUTPUT,
    WRITE,
    FLUSH
  } OperationType;

  /** Set the priority level for the current logger. Only messages that have
   * priorities equal or greater than the one set here will be posted to the
   * current outputs. */
  void
  SetPriorityLevel(PriorityLevelEnum level) override;

  /** Get the priority level for the current logger. Only messages that have
   * priorities equal or greater than the one set here will be posted to the
   * current outputs. */
  PriorityLevelEnum
  GetPriorityLevel() const override;

  void
  SetLevelForFlushing(PriorityLevelEnum level) override;

  PriorityLevelEnum
  GetLevelForFlushing() const override;

  /** Set the delay in milliseconds between checks to see if there are any
   *  low priority messages to be processed.
   */
  virtual void
  SetDelay(DelayType delay);

  /** Get the delay in milliseconds between checks to see if there are any
   *  low priority messages to be processed.
   */
  virtual DelayType
  GetDelay() const;

  /** Registers another output stream with the multiple output. */
  void
  AddLogOutput(OutputType * output) override;

  void
  Write(PriorityLevelEnum level, std::string const & content) override;

  void
  Flush() override;

protected:
  /** Constructor */
  ThreadLogger();

  /** Destructor */
  ~ThreadLogger() override;

  /** Print contents of a ThreadLogger */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  ThreadFunction();

private:
  void
  InternalFlush();

  using OperationContainerType = std::queue<OperationType>;

  using MessageContainerType = std::queue<std::string>;

  using LevelContainerType = std::queue<PriorityLevelEnum>;

  using OutputContainerType = std::queue<OutputType::Pointer>;

  std::thread m_Thread;

  bool m_TerminationRequested;

  OperationContainerType m_OperationQ;

  MessageContainerType m_MessageQ;

  LevelContainerType m_LevelQ;

  OutputContainerType m_OutputQ;

  mutable std::mutex m_Mutex;

  DelayType m_Delay;

}; // class ThreadLogger
} // namespace itk

#endif // itkThreadLogger_h
