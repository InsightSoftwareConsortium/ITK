/*=========================================================================

  Program:   Insight Segmentation & Registration Toolkit
  Module:    itkXMLFile.cxx
  Language:  C++
  Date:      $Date$
  Version:   $1.0$

  Copyright (c) 2002 Insight Consortium. All rights reserved.
  See ITKCopyright.txt or http://www.itk.org/HTML/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/
#ifndef __itkXMLFile_txx
#define __itkXMLFile_txx

#include "itkXMLFile.h"
#include "itkExceptionObject.h"
#include <itksys/SystemTools.hxx>
#include <fstream>

namespace itk
{

//----------------------------------------------------------------------------
static void itkXMLParserStartElement(void* parser, const char *name,
                              const char **atts)
{
  // Begin element handler that is registered with the XML_Parser.
  // This just casts the user data to a itkXMLParser and calls
  // StartElement.
  static_cast<XMLReaderBase*>(parser)->StartElement(name, atts);
}

//----------------------------------------------------------------------------
static void itkXMLParserEndElement(void* parser, const char *name)
{
  // End element handler that is registered with the XML_Parser.  This
  // just casts the user data to a itkXMLParser and calls EndElement.
  static_cast<XMLReaderBase*>(parser)->EndElement(name);
}

//----------------------------------------------------------------------------
static void itkXMLParserCharacterDataHandler(void* parser, const char* data,
                                      int length)
{
  // Character data handler that is registered with the XML_Parser.
  // This just casts the user data to a itkXMLParser and calls
  // CharacterDataHandler.
  static_cast<XMLReaderBase*>(parser)->CharacterDataHandler(data, length);
}

void
XMLReaderBase::
parse(void)
{
  XML_Parser Parser = XML_ParserCreate(0);

  XML_SetElementHandler(Parser,
                        &itkXMLParserStartElement,
                        &itkXMLParserEndElement);

  XML_SetCharacterDataHandler(Parser,
                              &itkXMLParserCharacterDataHandler);
  XML_SetUserData(Parser,this);

  std::ifstream inputstream;
  inputstream.open(m_Filename.c_str(),std::ios::in);
  if(inputstream.fail())
    {
    ExceptionObject exception(__FILE__, __LINE__);
    std::string message = "Can't open ";
    message += m_Filename;
    message += '\n';
    exception.SetDescription(message.c_str());
    throw exception;
    }

  // Default stream parser just reads a block at a time.
  std::streamsize filesize = 
    itksys::SystemTools::FileLength(m_Filename.c_str());
  char *buffer = new char [filesize];
  
  inputstream.read(buffer,filesize);
  if(inputstream.gcount() != filesize)
    {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("File Read Error");
    throw exception;
    }
  bool result = XML_Parse(Parser,buffer,inputstream.gcount(),false);
  delete [] buffer;
  if(!result)
    {
    ExceptionObject exception(__FILE__, __LINE__);
    
    std::string message(XML_ErrorString(XML_GetErrorCode(Parser)));
    message += " ";
    message += m_Filename;
    message += '\n';
    exception.SetDescription(message.c_str());
    throw exception;
    
    }
  return;
}

void
XMLReaderBase::
GenerateOutputInformation()
{
  this->parse();
}

}

#endif
