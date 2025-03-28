/*=========================================================================
 *
 *  Copyright NumFOCUS
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *         https://www.apache.org/licenses/LICENSE-2.0.txt
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
#include "ITKCommonExport.h"
#ifdef DEBUG
#  undef DEBUG // HDF5 publicly exports this define when built in debug mode
// That messes up the DEBUG enumeration in PriorityLevelEnum.
#endif

namespace itk
{
/*** \class LoggerBaseEnums
 * \brief Contains all enum classes used by LoggerBase class.
 * \ingroup ITKCommon
 */
class LoggerBaseEnums
{
public:
  /**
   *  \ingroup ITKCommon
   * Definition of types of messages. These codes will be used to regulate
   * the level of detail of messages reported to the final outputs
   */
  enum class PriorityLevel : uint8_t
  {
    MUSTFLUSH = 0,
    FATAL,
    CRITICAL,
    WARNING,
    INFO,
    DEBUG,
    NOTSET
  };

  /**
   * \ingroup ITKCommon
   * Select the type of format for reporting time stamps */
  enum class TimeStampFormat : uint8_t
  {
    REALVALUE = 0,
    HUMANREADABLE = 1
  };
};

// Define how to print enumeration
extern ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, const LoggerBaseEnums::PriorityLevel value);
extern ITKCommon_EXPORT std::ostream &
                        operator<<(std::ostream & out, const LoggerBaseEnums::TimeStampFormat value);

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

class ITKCommon_EXPORT LoggerBase : public Object
{
public:
  using Self = LoggerBase;
  using Superclass = Object;
  using Pointer = SmartPointer<Self>;
  using ConstPointer = SmartPointer<const Self>;

  /** \see LightObject::GetNameOfClass() */
  itkOverrideGetNameOfClassMacro(LoggerBase);

  using OutputType = MultipleLogOutput::OutputType;

  using PriorityLevelEnum = LoggerBaseEnums::PriorityLevel;
#if !defined(ITK_LEGACY_REMOVE)
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr PriorityLevelEnum MUSTFLUSH = PriorityLevelEnum::MUSTFLUSH;
  static constexpr PriorityLevelEnum FATAL = PriorityLevelEnum::FATAL;
  static constexpr PriorityLevelEnum CRITICAL = PriorityLevelEnum::CRITICAL;
  static constexpr PriorityLevelEnum WARNING = PriorityLevelEnum::WARNING;
  static constexpr PriorityLevelEnum INFO = PriorityLevelEnum::INFO;
  static constexpr PriorityLevelEnum DEBUG = PriorityLevelEnum::DEBUG;
  static constexpr PriorityLevelEnum NOTSET = PriorityLevelEnum::NOTSET;
#endif

  itkSetStringMacro(Name);
  itkGetStringMacro(Name);

  using TimeStampFormatEnum = LoggerBaseEnums::TimeStampFormat;
#if !defined(ITK_LEGACY_REMOVE)
  // We need to expose the enum values at the class level
  // for backwards compatibility
  static constexpr TimeStampFormatEnum REALVALUE = TimeStampFormatEnum::REALVALUE;
  static constexpr TimeStampFormatEnum HUMANREADABLE = TimeStampFormatEnum::HUMANREADABLE;
#endif

  /** Set/Get the type of format used for reporting the time stamp of a given
   * log message. The main options are REALVALUE and HUMANREADABLE.
   * REALVALUE will report the time in seconds as a double number.
   * HUMANREADABLE will report the time in formatted text such as '2007 May 7
   * 09:23:14'
   *
   * \sa SetHumanReadableFormat()
   *
   */
  /** @ITKStartGrouping */
  itkSetEnumMacro(TimeStampFormat, TimeStampFormatEnum);
  itkGetConstReferenceMacro(TimeStampFormat, TimeStampFormatEnum);
  /** @ITKEndGrouping */
  /** Set/Get the specific text format to use when the time stamp format type
   * is set to HUMANREADABLE. For a description of the acceptable formats
   * please look at the man page of the strftime() method. The default is set
   * to  "%Y %b %d %H:%M:%S"
   *
   * \sa SetTimeStampFormat
   *
   */
  /** @ITKStartGrouping */
  itkSetStringMacro(HumanReadableFormat);
  itkGetStringMacro(HumanReadableFormat);
  /** @ITKEndGrouping */
  /** Provides a default formatted log entry */
  virtual std::string
  BuildFormattedEntry(PriorityLevelEnum level, const std::string & content);

  /** Set the priority level for the current logger. Only messages that have
   * priorities equal or greater than the one set here will be posted to the
   * current outputs */
  virtual void
  SetPriorityLevel(PriorityLevelEnum level)
  {
    m_PriorityLevel = level;
  }

  /** Get the priority level for the current logger. Only messages that have
   * priorities equal or greater than the one set here will be posted to the
   * current outputs */
  virtual PriorityLevelEnum
  GetPriorityLevel() const
  {
    return m_PriorityLevel;
  }

  virtual void
  SetLevelForFlushing(PriorityLevelEnum level)
  {
    m_LevelForFlushing = level;
  }

  virtual PriorityLevelEnum
  GetLevelForFlushing() const
  {
    return m_LevelForFlushing;
  }

  /** Registers another output stream with the multiple output. */
  virtual void
  AddLogOutput(OutputType * output);

  virtual void
  Write(PriorityLevelEnum level, const std::string & content);

  /** Helper methods */
  void
  Debug(const std::string & message)
  {
    this->Write(LoggerBase::PriorityLevelEnum::DEBUG, message);
  }

  void
  Info(const std::string & message)
  {
    this->Write(LoggerBase::PriorityLevelEnum::INFO, message);
  }

  void
  Warning(const std::string & message)
  {
    this->Write(LoggerBase::PriorityLevelEnum::WARNING, message);
  }

  void
  Critical(const std::string & message)
  {
    this->Write(LoggerBase::PriorityLevelEnum::CRITICAL, message);
  }

  void
  Error(const std::string & message)
  {
    this->Write(LoggerBase::PriorityLevelEnum::CRITICAL, message);
  }

  void
  Fatal(const std::string & message)
  {
    this->Write(LoggerBase::PriorityLevelEnum::FATAL, message);
  }

  virtual void
  Flush();

protected:
  virtual void
  PrivateFlush();

  /** Constructor */
  LoggerBase();

  /** Destructor */
  ~LoggerBase() override;

  /** Print contents of a LoggerBase */
  void
  PrintSelf(std::ostream & os, Indent indent) const override;

protected:
  PriorityLevelEnum m_PriorityLevel{};

  PriorityLevelEnum m_LevelForFlushing{};

  MultipleLogOutput::Pointer m_Output{};

  RealTimeClock::Pointer m_Clock{};

  TimeStampFormatEnum m_TimeStampFormat{};

  std::string m_HumanReadableFormat{};

private:
  std::string m_Name{};
}; // class LoggerBase
} // namespace itk

#endif // itkLoggerBase_h
