/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkXMLFileOutputWindow.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

Copyright (c) 2001 Insight Consortium
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

 * Redistributions of source code must retain the above copyright notice,
   this list of conditions and the following disclaimer.

 * Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation
   and/or other materials provided with the distribution.

 * The name of the Insight Consortium, nor the names of any consortium members,
   nor of any contributors, may be used to endorse or promote products derived
   from this software without specific prior written permission.

  * Modified source versions must be plainly marked as such, and must not be
    misrepresented as being the original software.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDER AND CONTRIBUTORS ``AS IS''
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

=========================================================================*/
#include "itkXMLFileOutputWindow.h"
#include <fstream>

namespace itk
{
  
/**
 * Prompting off by default
 */
XMLFileOutputWindow
::XMLFileOutputWindow()
{
}

XMLFileOutputWindow
::~XMLFileOutputWindow()
{
}

void 
XMLFileOutputWindow
::PrintSelf(std::ostream& os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);
}


void
XMLFileOutputWindow
::Initialize() 
{
  if (!m_Stream)
    {
    if (m_FileName == "")
      {
      m_FileName = "itkMessageLog.xml";
      }
    if (m_Append)
      {
      m_Stream = new std::ofstream(m_FileName.c_str(), std::ios::app);
      }
    else
      {
      m_Stream = new std::ofstream(m_FileName.c_str());
      }
    }
}

void
XMLFileOutputWindow
::DisplayTag(const char* text)
{
  Superclass::DisplayText(text);
}


void
XMLFileOutputWindow
::DisplayXML(const char* tag, const char* text)
{
  char *xmlText;

  if(!text)
    {
    return;
    }

  // allocate enough room for the worst case
  xmlText = new char[strlen(text) * 6 + 1];

  const char *s = text;
  char *x = xmlText;
  *x = '\0';

  // replace all special characters
  while (*s)
    {
    switch (*s)
      {
      case '&':
	strcat(x, "&amp;"); x += 5;
	break;
      case '"':
	strcat(x, "&quot;"); x += 6;
	break;
      case '\'':
	strcat(x, "&apos;"); x += 6;
	break;
      case '<':
	strcat(x, "&lt;"); x += 4;
	break;
      case '>':
	strcat(x, "&gt;"); x += 4;
	break;
      default:
	*x = *s; x++;
	*x = '\0'; // explicitly terminate the new string
      }
    s++;
    }

  if (!m_Stream)
    {
    this->Initialize();
    }
  *m_Stream << "<" << tag << ">" << xmlText << "</" << tag << ">" << std::endl;
  
  if (m_Flush)
    {
    m_Stream->flush();
    }
  delete []xmlText;
}

void
XMLFileOutputWindow
::DisplayText(const char* text)
{
  this->DisplayXML("Text", text);
}

void
XMLFileOutputWindow
::DisplayErrorText(const char* text)
{
  this->DisplayXML("Error", text);
}

void
XMLFileOutputWindow
::DisplayWarningText(const char* text)
{
  this->DisplayXML("Warning", text);
}

void
XMLFileOutputWindow
::DisplayGenericOutputText(const char* text)
{
  this->DisplayXML("GenericOutput", text);
}

void
XMLFileOutputWindow
::DisplayDebugText(const char* text)
{
  this->DisplayXML("Debug", text);
}

} // end namespace itk
