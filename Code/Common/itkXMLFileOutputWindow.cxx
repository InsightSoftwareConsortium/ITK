/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkXMLFileOutputWindow.cxx
  Language:  C++
  Date:      $Date$
  Version:   $Revision$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifdef _WIN32
#pragma warning ( disable : 4786 )
#endif

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
  delete [] xmlText;
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
