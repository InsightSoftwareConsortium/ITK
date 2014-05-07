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
  m_Stream = ITK_NULLPTR;
  m_FileName = "";
}

FileOutputWindow
::~FileOutputWindow()
{
  delete m_Stream;
  m_Stream = ITK_NULLPTR;
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
