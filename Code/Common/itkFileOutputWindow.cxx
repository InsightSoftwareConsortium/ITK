/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkFileOutputWindow.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) Insight Software Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#include "itkFileOutputWindow.h"

namespace itk
{
/**
 * Prompting off by default
 */
FileOutputWindow
::FileOutputWindow()
{
  m_Flush = false;
  m_Append = false;
  m_Stream = 0;
  m_FileName = "";
}

FileOutputWindow
::~FileOutputWindow()
{
  if ( m_Stream )
    {
    delete m_Stream;
    m_Stream = 0;
    }
}

void
FileOutputWindow
::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "FileName: " << m_FileName << std::endl;
  os << indent << "Stream: " << m_Stream << std::endl;
  os << indent << "Append: " << ( m_Append ? "On\n" : "Off\n" ) << std::endl;
  os << indent << "Flush: " << ( m_Flush ? "On\n" : "Off\n" ) << std::endl;
}

void
FileOutputWindow
::Initialize()
{
  if ( !m_Stream )
    {
    if ( m_FileName == "" )
      {
      m_FileName = "itkMessageLog.txt";
      }
    if ( m_Append )
      {
      m_Stream = new std::ofstream(m_FileName.c_str(), std::ios::app);
      }
    else
      {
      m_Stream = new std::ofstream( m_FileName.c_str() );
      }
    }
}

/**
 *
 */
void
FileOutputWindow
::DisplayText(const char *txt)
{
  if ( !txt )
    {
    return;
    }

  if ( !m_Stream )
    {
    this->Initialize();
    }
  *m_Stream << txt << std::endl;

  if ( m_Flush )
    {
    m_Stream->flush();
    }
}
} // end namespace itk
