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
#ifndef itkLoggerBase_h
#define itkLoggerBase_h

#include "itkMultipleLogOutput.h"
#include "itkRealTimeClock.h"

namespace itk
{
/** \class LoggerBase
 *  \brief Used for logging information during a run.
 *
 * \author Hee-Su Kim, Compute Science Dept. Kyungpook National University,
 *                     ISIS Center, Georgetown University.
 *
 *
 * \ingroup OSSystemObjects LoggingObjects
 * \ingroup ITKCommon
 */

class ITKCommon_EXPORT LoggerBase:public Object
{
public:

  typedef LoggerBase                 Self;
  typedef Object                     Superclass;
  typedef SmartPointer< Self >       Pointer;
  typedef SmartPointer< const Self > ConstPointer;

  /** Run-time type information (and related methods). */
  itkTypeMacro(LoggerBase, Object);

  typedef  MultipleLogOutput::OutputType OutputType;

  /** Definition of types of messages. These codes will be used to regulate
    * the level of detail of messages reported to the final outputs */
  typedef enum {
    MUSTFLUSH = 0,
    FATAL,
    CRITICAL,
    WARNING,
    INFO,
    DEBUG,
    NOTSET
    } PriorityLevelType;

  itkSetStringMacro(Name);
  itkGetStringMacro(Name);

  /** Select the type of format for reporting time stamps */
  typedef enum {
    REALVALUE = 0,
    HUMANREADABLE
    } TimeStampFormatType;

  /** Set/Get the type of format used for reporting the time stamp of a given
   * log message. The main options are REALVALUE and HUMANREADABLE.
   * REALVALUE will report the time in seconds as a double number.
   * HUMANREADABLE will report the time in formatted text such as '2007 May 7
   * 09:23:14'
   *
   * \sa SetHumanReadableFormat()
   *
   */
  itkSetMacro(TimeStampFormat, TimeStampFormatType);
  itkGetConstReferenceMacro(TimeStampFormat, TimeStampFormatType);

  /** Set/Get the specific text format to use when the time stamp format type
   * is set to HUMANREADABLE. For a description of the acceptable formats
   * please look at the man page of the strftime() method. The default is set
   * to  "%Y %b %d %H:%M:%S"
   *
   * \sa SetTimeStampFormat
   *
   */
  itkSetStringMacro(HumanReadableFormat);
  itkGetStringMacro(HumanReadableFormat);

  /** Provides a default formatted log entry */
  virtual std::string BuildFormattedEntry(PriorityLevelType level,
                                          std::string const & content);

  /** Set the priority level for the current logger. Only messages that have
    * priorities equal or greater than the one set here will be posted to the
    * current outputs */
  virtual void SetPriorityLevel(PriorityLevelType level)
  {
    m_PriorityLevel = level;
  }

  /** Get the priority level for the current logger. Only messages that have
   * priorities equal or greater than the one set here will be posted to the
   * current outputs */
  virtual PriorityLevelType GetPriorityLevel() const
  {
    return m_PriorityLevel;
  }

  virtual void SetLevelForFlushing(PriorityLevelType level)
  {
    m_LevelForFlushing = level;
  }

  virtual PriorityLevelType GetLevelForFlushing() const
  {
    return m_LevelForFlushing;
  }

  /** Registers another output stream with the multiple output. */
  virtual void AddLogOutput(OutputType *output);

  virtual void Write(PriorityLevelType level, std::string const & content);

  /** Helper methods */
  void Debug(std::string const & message)
  {
    this->Write (LoggerBase::DEBUG, message);
  }

  void Info(std::string const & message)
  {
    this->Write (LoggerBase::INFO, message);
  }

  void Warning(std::string const & message)
  {
    this->Write (LoggerBase::WARNING, message);
  }

  void Critical(std::string const & message)
  {
    this->Write (LoggerBase::CRITICAL, message);
  }

  void Error(std::string const & message)
  {
    this->Write (LoggerBase::CRITICAL, message);
  }

  void Fatal(std::string const & message)
  {
    this->Write (LoggerBase::FATAL, message);
  }

  virtual void Flush();

protected:

  /** Constructor */
  LoggerBase();

  /** Destructor */
  virtual ~LoggerBase() ITK_OVERRIDE;

  /** Print contents of a LoggerBase */
  virtual void PrintSelf(std::ostream & os, Indent indent) const ITK_OVERRIDE;

protected:

  PriorityLevelType m_PriorityLevel;

  PriorityLevelType m_LevelForFlushing;

  MultipleLogOutput::Pointer m_Output;

  RealTimeClock::Pointer m_Clock;

  TimeStampFormatType m_TimeStampFormat;

  std::string m_HumanReadableFormat;

private:

  std::string m_Name;
};  // class LoggerBase
} // namespace itk

#endif  // itkLoggerBase_h
