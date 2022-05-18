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
#include "itkLoggerBase.h"
#include "itksys/SystemTools.hxx"

namespace itk
{
LoggerBase::LoggerBase()
{
  this->m_PriorityLevel = LoggerBase::PriorityLevelEnum::NOTSET;
  this->m_LevelForFlushing = LoggerBase::PriorityLevelEnum::MUSTFLUSH;
  this->m_Clock = RealTimeClock::New();
  this->m_Output = MultipleLogOutput::New();
  this->m_TimeStampFormat = TimeStampFormatEnum::REALVALUE;
  this->m_HumanReadableFormat = "%Y %b %d %H:%M:%S";
}

LoggerBase::~LoggerBase()
{
  //  this->m_Output->Flush();
}

/** Adds an output stream to the MultipleLogOutput for writing. */
void
LoggerBase::AddLogOutput(OutputType * output)
{
  // delegates to MultipleLogOutput
  this->m_Output->AddLogOutput(output);
}

void
LoggerBase::Write(PriorityLevelEnum level, std::string const & content)
{
  if (this->m_PriorityLevel >= level)
  {
    this->m_Output->Write(this->BuildFormattedEntry(level, content));
    if (this->m_LevelForFlushing >= level)
    {
      this->m_Output->Flush();
    }
  }
}

void
LoggerBase::Flush()
{
  this->PrivateFlush();
}

void
LoggerBase::PrivateFlush()
{
  this->m_Output->Flush();
}

std::string
LoggerBase::BuildFormattedEntry(PriorityLevelEnum level, std::string const & content)
{
  static std::string m_LevelString[] = { "(MUSTFLUSH) ", "(FATAL) ", "(CRITICAL) ", "(WARNING) ",
                                         "(INFO) ",      "(DEBUG) ", "(NOTSET) " };
  std::ostringstream s;

  switch (this->m_TimeStampFormat)
  {
    case TimeStampFormatEnum::REALVALUE:
    {
      s.precision(30);
      s << m_Clock->GetTimeInSeconds();
      break;
    }
    case TimeStampFormatEnum::HUMANREADABLE:
    {
      s << itksys::SystemTools::GetCurrentDateTime(this->m_HumanReadableFormat.c_str());
      break;
    }
  }
  s << "  :  " << this->GetName() << "  " << m_LevelString[static_cast<int>(level)] << content;

  return s.str();
}

/** Print contents of a LoggerBase */
void
LoggerBase::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Name: " << this->GetName() << std::endl;
  os << indent << "PriorityLevel: " << this->GetPriorityLevel() << std::endl;
  os << indent << "LevelForFlushing: " << this->GetLevelForFlushing() << std::endl;
  os << indent << "TimeStampFormat: " << this->GetTimeStampFormat() << std::endl;
  os << indent << "HumanReadableFormat: " << this->GetHumanReadableFormat() << std::endl;
}

/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const LoggerBaseEnums::PriorityLevel value)
{
  return out << [value] {
    switch (value)
    {
      case LoggerBaseEnums::PriorityLevel::MUSTFLUSH:
        return "itk::LoggerBaseEnums::PriorityLevel::MUSTFLUSH";
      case LoggerBaseEnums::PriorityLevel::FATAL:
        return "itk::LoggerBaseEnums::PriorityLevel::FATAL";
      case LoggerBaseEnums::PriorityLevel::CRITICAL:
        return "itk::LoggerBaseEnums::PriorityLevel::CRITICAL";
      case LoggerBaseEnums::PriorityLevel::WARNING:
        return "itk::LoggerBaseEnums::PriorityLevel::WARNING";
      case LoggerBaseEnums::PriorityLevel::INFO:
        return "itk::LoggerBaseEnums::PriorityLevel::INFO";
      case LoggerBaseEnums::PriorityLevel::DEBUG:
        return "itk::LoggerBaseEnums::PriorityLevel::DEBUG";
      case LoggerBaseEnums::PriorityLevel::NOTSET:
        return "itk::LoggerBaseEnums::PriorityLevel::NOTSET";
      default:
        return "INVALID VALUE FOR itk::LoggerBaseEnums::PriorityLevel";
    }
  }();
}

/** Print enum values */
std::ostream &
operator<<(std::ostream & out, const LoggerBaseEnums::TimeStampFormat value)
{
  return out << [value] {
    switch (value)
    {
      case LoggerBaseEnums::TimeStampFormat::REALVALUE:
        return "itk::LoggerBaseEnums::TimeStampFormat::REALVALUE";
      case LoggerBaseEnums::TimeStampFormat::HUMANREADABLE:
        return "itk::LoggerBaseEnums::TimeStampFormat::HUMANREADABLE";
      default:
        return "INVALID VALUE FOR itk::LoggerBaseEnums::TimeStampFormat";
    }
  }();
}
} // namespace itk
