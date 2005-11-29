/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLoggerBase.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "itkLoggerBase.h"

namespace itk
{

LoggerBase::LoggerBase()
{
  this->m_PriorityLevel = LoggerBase::NOTSET;
  this->m_LevelForFlushing = LoggerBase::MUSTFLUSH;
  this->m_Clock = RealTimeClock::New();
  this->m_Output = MultipleLogOutput::New();
}

LoggerBase::~LoggerBase()
{
//  this->m_Output->Flush();
}

/** Adds an output stream to the MultipleLogOutput for writing. */
void LoggerBase::AddLogOutput( OutputType* output )
{
  // delegates to MultipleLogOutput
  this->m_Output->AddLogOutput( output ); 
}

void LoggerBase::Write(PriorityLevelType level, std::string const & content)
{
  if( this->m_PriorityLevel >= level )
  {
    this->m_Output->Write(this->BuildFormattedEntry(level,content));
    if( this->m_LevelForFlushing >= level )
    {
      this->m_Output->Flush();
    }
  }
}

void LoggerBase::Flush()
{
  this->m_Output->Flush();
}

std::string LoggerBase::BuildFormattedEntry(PriorityLevelType level, std::string const & content)
{
    static std::string m_LevelString[] = { "(MUSTFLUSH) ", "(FATAL) ", "(ERROR) ",
        "(WARNING) ", "(INFO) ", "(DEBUG) ", "(NOTSET) " };
    OStringStream s;
    s.precision(30);
    s << m_Clock->GetTimeStamp() << "  :  " << this->GetName() <<  "  " <<  m_LevelString[level] << content;
    return s.str();
}

/** Print contents of a LoggerBase */
void LoggerBase::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);

  os << indent << "Name: " << this->GetName() << std::endl;
  os << indent << "PriorityLevel: " << this->GetPriorityLevel()   << std::endl;
  os << indent << "LevelForFlushing: " << this->GetLevelForFlushing() << std::endl;
}

} //namespace

