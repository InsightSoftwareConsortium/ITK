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
#ifndef itkLoggerThreadWrapper_h
#define itkLoggerThreadWrapper_h

#include <string>
#include <queue>

#include "itkMultiThreader.h"
#include "itkSimpleFastMutexLock.h"

namespace itk
{
/** \class LoggerThreadWrapper
 *  \brief Used for providing logging service as a separate thread.
 *
 * \author Hee-Su Kim, Compute Science Dept. Kyungpook National University,
 *                     ISIS Center, Georgetown University.
 *
 * \ingroup OSSystemObjects LoggingObjects
 * \ingroup ITKCommon
 */

template< typename SimpleLoggerType >
class ITK_TEMPLATE_EXPORT LoggerThreadWrapper:public SimpleLoggerType
{
public:

  typedef LoggerThreadWrapper        Self;
  typedef SimpleLoggerType           Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(LoggerThreadWrapper, SimpleLoggerType);

  /** New macro for creation of through a Smart Pointer */
  itkNewMacro(Self);

  typedef  typename SimpleLoggerType::OutputType        OutputType;
  typedef  typename SimpleLoggerType::PriorityLevelType PriorityLevelType;
  typedef  unsigned int                                 DelayType;

  /** Definition of types of operations for LoggerThreadWrapper. */
  typedef enum {
    SET_PRIORITY_LEVEL,
    SET_LEVEL_FOR_FLUSHING,
    ADD_LOG_OUTPUT,
    WRITE
    } OperationType;

  /** Set the priority level for the current logger. Only messages that have
   * priorities equal or greater than the one set here will be posted to the
   * current outputs */
  virtual void SetPriorityLevel(PriorityLevelType level) ITK_OVERRIDE;

  /** Get the priority level for the current logger. Only messages that have
   * priorities equal or greater than the one set here will be posted to the
   * current outputs */
  virtual PriorityLevelType GetPriorityLevel() const ITK_OVERRIDE;

  virtual void SetLevelForFlushing(PriorityLevelType level) ITK_OVERRIDE;

  virtual PriorityLevelType GetLevelForFlushing() const ITK_OVERRIDE;

/** Set the delay in milliseconds between checks to see if there are any
 *  low priority messages to be processed.
 */
  virtual void SetDelay(DelayType delay);

/** Get the delay in milliseconds between checks to see if there are any
 *  low priority messages to be processed.
 */
  virtual DelayType GetDelay() const;

  /** Registers another output stream with the multiple output. */
  virtual void AddLogOutput(OutputType *output) ITK_OVERRIDE;

  virtual void Write(PriorityLevelType level, std::string const & content) ITK_OVERRIDE;

  virtual void Flush() ITK_OVERRIDE;

protected:

  /** Constructor */
  LoggerThreadWrapper();

  /** Destructor */
  virtual ~LoggerThreadWrapper() ITK_OVERRIDE;

  /** Print contents of a LoggerThreadWrapper */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

  static ITK_THREAD_RETURN_TYPE ThreadFunction(void *);

private:

  typedef std::queue< OperationType > OperationContainerType;

  typedef std::queue< std::string > MessageContainerType;

  typedef std::queue< PriorityLevelType > LevelContainerType;

  typedef std::queue< typename OutputType::Pointer > OutputContainerType;

  MultiThreader::Pointer m_Threader;

  ThreadIdType m_ThreadID;

  OperationContainerType m_OperationQ;

  MessageContainerType m_MessageQ;

  LevelContainerType m_LevelQ;

  OutputContainerType m_OutputQ;

  SimpleFastMutexLock m_Mutex;

  DelayType m_Delay;

};  // class LoggerThreadWrapper
} // namespace itk

#ifndef ITK_MANUAL_INSTANTIATION
#include "itkLoggerThreadWrapper.hxx"
#endif

#endif  // itkLoggerThreadWrapper_h
