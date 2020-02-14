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
#ifndef itkLoggerThreadWrapper_h
#define itkLoggerThreadWrapper_h

#include <string>
#include <queue>
#include <thread>

#include "itkObjectFactory.h"
#include <mutex>
#include "ITKCommonExport.h"

namespace itk
{

/** \class LoggerThreadWrapperEnums
 * \brief enums for LoggerThreadWrapper
 * \ingroup ITKCommon
 */
class LoggerThreadWrapperEnums
{
public:
  /** \class Operation
   * \ingroup ITKCommon
   * Definition of types of operations for LoggerThreadWrapper.
   */
  enum class Operation : uint8_t
  {
    SET_PRIORITY_LEVEL,
    SET_LEVEL_FOR_FLUSHING,
    ADD_LOG_OUTPUT,
    WRITE
  };
};
// Define how to print enumeration
extern ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, const LoggerThreadWrapperEnums::Operation value);


/** \class LoggerThreadWrapper
 *  \brief Used for providing logging service as a separate thread.
 *
 * \author Hee-Su Kim, Compute Science Dept. Kyungpook National University,
 *                     ISIS Center, Georgetown University.
 *
 * \ingroup OSSystemObjects LoggingObjects
 * \ingroup ITKCommon
 */

template <typename SimpleLoggerType>
class ITK_TEMPLATE_EXPORT LoggerThreadWrapper : public SimpleLoggerType
{
public:
  using Self = LoggerThreadWrapper;
  using Superclass = SimpleLoggerType;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** Run-time type information (and related methods). */
  itkTypeMacro(LoggerThreadWrapper, SimpleLoggerType);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  using OutputType = typename SimpleLoggerType::OutputType;
  using PriorityLevelEnum = typename SimpleLoggerType::PriorityLevelEnum;
  using DelayType = unsigned int;

  using OperationEnum = LoggerThreadWrapperEnums::Operation;

#if !defined(ITK_LEGACY_REMOVE)
  using LoggerThreadWrapperOperationType = OperationEnum;

  static constexpr OperationEnum SET_PRIORITY_LEVEL = OperationEnum::SET_PRIORITY_LEVEL;
  static constexpr OperationEnum SET_LEVEL_FOR_FLUSHING = OperationEnum::SET_LEVEL_FOR_FLUSHING;
  static constexpr OperationEnum ADD_LOG_OUTPUT = OperationEnum::ADD_LOG_OUTPUT;
  static constexpr OperationEnum WRITE = OperationEnum::WRITE;
#endif

  /** Set the priority level for the current logger. Only messages that have
   * priorities equal or greater than the one set here will be posted to the
   * current outputs */
  void
  SetPriorityLevel(PriorityLevelEnum level) override;

  /** Get the priority level for the current logger. Only messages that have
   * priorities equal or greater than the one set here will be posted to the
   * current outputs */
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
  LoggerThreadWrapper();

  /** Destructor */
  ~LoggerThreadWrapper() override;

  /** Print contents of a LoggerThreadWrapper */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

  void
  ThreadFunction();

private:
  using OperationContainerType = std::queue<OperationEnum>;

  using MessageContainerType = std::queue<std::string>;

  using LevelContainerType = std::queue<PriorityLevelEnum>;

  using OutputContainerType = std::queue<typename OutputType::Pointer>;

  std::thread m_Thread;

  bool m_TerminationRequested;

  OperationContainerType m_OperationQ;

  MessageContainerType m_MessageQ;

  LevelContainerType m_LevelQ;

  OutputContainerType m_OutputQ;

  mutable std::mutex m_Mutex;

  DelayType m_Delay;

}; // class LoggerThreadWrapper

} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#  include "itkLoggerThreadWrapper.hxx"
#endif

#endif // itkLoggerThreadWrapper_h
