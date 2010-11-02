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
/*=========================================================================
 *
 *  Portions of this file are subject to the VTK Toolkit Version 3 copyright.
 *
 *  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
 *
 *  For complete copyright, license and disclaimer of warranty information
 *  please refer to the NOTICE file at the top of the ITK source tree.
 *
 *=========================================================================*/
#include "itkLoggerOutput.h"

namespace itk
{
/** Send a string to display. */
void LoggerOutput::DisplayText(const char *t)
{
  if ( this->m_Logger )
    {
    this->m_Logger->Write(LoggerBase::INFO, t);
    }
}

/** Send a string as an error message to display.
 * The default implementation calls DisplayText() but subclasses
 * could present this message differently. */
void LoggerOutput::DisplayErrorText(const char *t)
{
  if ( this->m_Logger )
    {
    this->m_Logger->Write(LoggerBase::CRITICAL, t);
    }
}

/** Send a string as a warningmessage to display.
 * The default implementation calls DisplayText() but subclasses
 * could present this message differently. */
void LoggerOutput::DisplayWarningText(const char *t)
{
  if ( this->m_Logger )
    {
    this->m_Logger->Write(LoggerBase::WARNING, t);
    }
}

/** Send a string as a message to display.
 * The default implementation calls DisplayText() but subclasses
 * could present this message differently. */
void LoggerOutput::DisplayGenericOutputText(const char *t)
{
  if ( this->m_Logger )
    {
    this->m_Logger->Write(LoggerBase::INFO, t);
    }
}

/** Send a string as a debug message to display.
 * The default implementation calls DisplayText() but subclasses
 * could present this message differently. */
void LoggerOutput::DisplayDebugText(const char *t)
{
  if ( this->m_Logger )
    {
    this->m_Logger->Write(LoggerBase::DEBUG, t);
    }
}

void LoggerOutput::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
  os << indent << "Logger: " << m_Logger << std::endl;
}
} // end namespace itk
