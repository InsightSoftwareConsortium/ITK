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
#include "itkLoggerBase.h"
#include "itksys/SystemTools.hxx"

namespace itk
{
LoggerBase::LoggerBase()
{
  this->m_PriorityLevel = LoggerBase::NOTSET;
  this->m_LevelForFlushing = LoggerBase::MUSTFLUSH;
  this->m_Clock = RealTimeClock::New();
  this->m_Output = MultipleLogOutput::New();
  this->m_TimeStampFormat = REALVALUE;
  this->m_HumanReadableFormat = "%Y %b %d %H:%M:%S";
}

LoggerBase::~LoggerBase()
{
//  this->m_Output->Flush();
}

/** Adds an output stream to the MultipleLogOutput for writing. */
void LoggerBase::AddLogOutput(OutputType *output)
{
  // delegates to MultipleLogOutput
  this->m_Output->AddLogOutput(output);
}

void LoggerBase::Write(PriorityLevelType level, std::string const & content)
{
  if ( this->m_PriorityLevel >= level )
    {
    this->m_Output->Write( this->BuildFormattedEntry(level, content) );
    if ( this->m_LevelForFlushing >= level )
      {
      this->m_Output->Flush();
      }
    }
}

void LoggerBase::Flush()
{
  this->m_Output->Flush();
}

std::string
LoggerBase
::BuildFormattedEntry(PriorityLevelType level, std::string const & content)
{
  static std::string m_LevelString[] = { "(MUSTFLUSH) ", "(FATAL) ", "(CRITICAL) ",
                                         "(WARNING) ", "(INFO) ", "(DEBUG) ", "(NOTSET) " };
  std::ostringstream s;

  switch ( this->m_TimeStampFormat )
    {
    case REALVALUE:
      {
      s.precision(30);
      s << m_Clock->GetTimeInSeconds();
      break;
      }
    case HUMANREADABLE:
      {
      s << itksys::SystemTools::GetCurrentDateTime( this->m_HumanReadableFormat.c_str() );
      break;
      }
    }
  s << "  :  " << this->GetName() <<  "  " <<  m_LevelString[level] << content;

  return s.str();
}

/** Print contents of a LoggerBase */
void LoggerBase::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Name: " << this->GetName() << std::endl;
  os << indent << "PriorityLevel: " << this->GetPriorityLevel()   << std::endl;
  os << indent << "LevelForFlushing: " << this->GetLevelForFlushing() << std::endl;
  os << indent << "TimeStampFormat: " << this->GetTimeStampFormat() << std::endl;
  os << indent << "HumanReadableFormat: " << this->GetHumanReadableFormat() << std::endl;
}
} //namespace
