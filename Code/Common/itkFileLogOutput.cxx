/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFileLogOutput.cxx
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
#include"itkFileLogOutput.h"


namespace itk
{

/** Constructor */
FileLogOutput::FileLogOutput()
{
  this->m_File = 0;
}

/** Destructor */
FileLogOutput::~FileLogOutput()
{
  if( this->m_File )
    {
    this->m_File->close();
    }
}


/** Set file stream */
void FileLogOutput::SetFile( FileType & FileStream )
{
  this->m_File = &FileStream;
  this->m_File->precision(15);
}


/** flush a buffer */
void FileLogOutput::Flush()
{
  FileLogOutput::m_Mutex.Lock();
  if( this->m_File )
    {
    this->m_File->flush();
    }
  FileLogOutput::m_Mutex.Unlock();
}


/** Write to multiple outputs */
void FileLogOutput::Write(double timestamp)
{
  FileLogOutput::m_Mutex.Lock();
  if( this->m_File )
    {
    (*this->m_File) << timestamp;
    }
  FileLogOutput::m_Mutex.Unlock();
}


/** Write to a buffer */
void FileLogOutput::Write( const std::string & content )
{
  FileLogOutput::m_Mutex.Lock();
  if( this->m_File )
    {
    (*this->m_File) << content;
    }
  FileLogOutput::m_Mutex.Unlock();
}


/** Write to a buffer */
void FileLogOutput::Write( const std::string & content, double timestamp )
{
  FileLogOutput::m_Mutex.Lock();
  if( this->m_File )
    {
    (*this->m_File) << timestamp << "  :  " << content;
    }
  FileLogOutput::m_Mutex.Unlock();
}

void FileLogOutput::PrintSelf(std::ostream &os, Indent indent) const
{
  Superclass::PrintSelf(os,indent);
  os << indent << "FileType: " << m_File << std::endl;;
}

}

