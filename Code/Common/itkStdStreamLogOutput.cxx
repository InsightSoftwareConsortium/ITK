/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkStdStreamLogOutput.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include<iostream>
#include"itkStdStreamLogOutput.h"


namespace itk
{

/** Constructor */
StdStreamLogOutput::StdStreamLogOutput()
{
  this->m_Stream = 0;
}

/** Destructor */
StdStreamLogOutput::~StdStreamLogOutput()
{
  if( this->m_Stream )
  {
    this->m_Stream->flush();
  }
}


/** Set file stream */
void StdStreamLogOutput::SetStream(StreamType &Stream)
{
  this->m_Stream = &Stream;
  this->m_Stream->precision(30);
}


/** flush a buffer */
void StdStreamLogOutput::Flush()
{
  StdStreamLogOutput::m_Mutex.Lock();
  if( this->m_Stream )
  {
    this->m_Stream->flush();
  }
  StdStreamLogOutput::m_Mutex.Unlock();
}


/** Write to a buffer */
void StdStreamLogOutput::Write(double timestamp)
{
  StdStreamLogOutput::m_Mutex.Lock();
  if( this->m_Stream )
  {
    (*this->m_Stream) << timestamp;
  }
  StdStreamLogOutput::m_Mutex.Unlock();
}


/** Write to a buffer */
void StdStreamLogOutput::Write(std::string const &content)
{
  StdStreamLogOutput::m_Mutex.Lock();
  if( this->m_Stream )
  {
    (*this->m_Stream) << content;
  }
  StdStreamLogOutput::m_Mutex.Unlock();
}


/** Write to a buffer */
void StdStreamLogOutput::Write(std::string const &content, double timestamp)
{
  StdStreamLogOutput::m_Mutex.Lock();
  if( this->m_Stream )
  {
    (*this->m_Stream) << timestamp << "  :  " << content;
  }
  StdStreamLogOutput::m_Mutex.Unlock();
}


}

