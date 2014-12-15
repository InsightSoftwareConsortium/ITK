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
#include "itkXMLFile.h"
#include "itksys/SystemTools.hxx"
#include <fstream>
#include "expat.h"

namespace itk
{
//----------------------------------------------------------------------------
extern "C"
{
static void itkXMLParserStartElement(void *parser, const char *name,
                                     const char **atts)
{
  // Begin element handler that is registered with the XML_Parser.
  // This just casts the user data to a itkXMLParser and calls
  // StartElement.
  static_cast< XMLReaderBase * >( parser )->StartElement(name, atts);
}
}

//----------------------------------------------------------------------------
extern "C" {
static void itkXMLParserEndElement(void *parser, const char *name)
{
  // End element handler that is registered with the XML_Parser.  This
  // just casts the user data to a itkXMLParser and calls EndElement.
  static_cast< XMLReaderBase * >( parser )->EndElement(name);
}
}

//----------------------------------------------------------------------------
extern "C" {
static void itkXMLParserCharacterDataHandler(void *parser, const char *data,
                                             int length)
{
  // Character data handler that is registered with the XML_Parser.
  // This just casts the user data to a itkXMLParser and calls
  // CharacterDataHandler.
  static_cast< XMLReaderBase * >( parser )->CharacterDataHandler(data, length);
}
}

void
XMLReaderBase::parse(void)
{
  XML_Parser Parser = XML_ParserCreate(ITK_NULLPTR);

  XML_SetElementHandler(Parser,
                        &itkXMLParserStartElement,
                        &itkXMLParserEndElement);

  XML_SetCharacterDataHandler(Parser,
                              &itkXMLParserCharacterDataHandler);
  XML_SetUserData(Parser, this);

  std::ifstream inputstream;

  inputstream.open(m_Filename.c_str(), std::ios::binary | std::ios::in);
  if ( inputstream.fail() )
    {
    ExceptionObject exception(__FILE__, __LINE__);
    std::string     message = "Can't open ";
    message += m_Filename;
    message += '\n';
    exception.SetDescription( message.c_str() );
    throw exception;
    }

  // Default stream parser just reads a block at a time.
  std::streamsize filesize =
    itksys::SystemTools::FileLength( m_Filename.c_str() );

  char *buffer = new char[filesize];

  inputstream.read(buffer, filesize);

  if ( static_cast< std::streamsize >( inputstream.gcount() ) != filesize )
    {
    ExceptionObject exception(__FILE__, __LINE__);
    exception.SetDescription("File Read Error");
    throw exception;
    }
  const bool result = static_cast<bool>(XML_Parse(Parser, buffer, inputstream.gcount(), false));
  delete[] buffer;
  if ( !result )
    {
    ExceptionObject exception(__FILE__, __LINE__);

    std::string message( XML_ErrorString( XML_GetErrorCode(Parser) ) );
    message += " ";
    message += m_Filename;
    message += '\n';
    exception.SetDescription( message.c_str() );
    throw exception;
    }
  XML_ParserFree(Parser);
}

void
XMLReaderBase::GenerateOutputInformation()
{
  this->parse();
}

void
XMLReaderBase::PrintSelf(std::ostream & os, Indent indent) const
{
  Superclass::PrintSelf(os, indent);

  os << indent << "Filename: " << m_Filename << std::endl;
}

} // namespace itk
