/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkLoggerOutput.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include"itkLoggerOutput.h"


namespace itk
{
/** Send a string to display. */
void LoggerOutput::DisplayText(const char* t)
{
  if( this->m_Logger )
  {
    this->m_Logger->Write(Logger::INFO, t);
  }
}


/** Send a string as an error message to display.
 * The default implementation calls DisplayText() but subclasses
 * could present this message differently. */
void LoggerOutput::DisplayErrorText(const char *t)
{
  if( this->m_Logger )
  {
    this->m_Logger->Write(Logger::CRITICAL, t);
  }
}


/** Send a string as a warningmessage to display.
 * The default implementation calls DisplayText() but subclasses
 * could present this message differently. */
void LoggerOutput::DisplayWarningText(const char *t)
{
  if( this->m_Logger )
  {
    this->m_Logger->Write(Logger::WARNING, t);
  }
}


/** Send a string as a message to display.
 * The default implementation calls DisplayText() but subclasses
 * could present this message differently. */
void LoggerOutput::DisplayGenericOutputText(const char *t)
{
  if( this->m_Logger )
  {
    this->m_Logger->Write(Logger::INFO, t);
  }
}


/** Send a string as a debug message to display.
 * The default implementation calls DisplayText() but subclasses
 * could present this message differently. */
void LoggerOutput::DisplayDebugText(const char *t)
{
  if( this->m_Logger )
  {
    this->m_Logger->Write(Logger::DEBUG, t);
  }
}

void LoggerOutput::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "Logger: " << m_Logger << std::endl;;
}
  
} // end namespace itk


